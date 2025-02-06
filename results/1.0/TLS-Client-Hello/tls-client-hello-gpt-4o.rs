use nom::{
    bytes::complete::take,
    combinator::{map, opt},
    multi::length_data,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, process};

#[derive(Debug)]
struct ClientHello<'a> {
    client_version: u16,
    random: &'a [u8],
    legacy_session_id: &'a [u8],
    cipher_suites: Vec<u16>,
    legacy_compression_methods: Vec<u8>,
    extensions: Vec<Extension<'a>>,
}

#[derive(Debug)]
enum Extension<'a> {
    SupportedVersions(Vec<u16>),
    SignatureAlgorithms(Vec<u16>),
    SupportedGroups(Vec<u16>),
    KeyShare(KeyShare<'a>),
    PreSharedKey(&'a [u8]),
    PskKeyExchangeModes(Vec<u8>),
    EarlyData,
    ServerName(&'a [u8]),
    Unknown(u16, &'a [u8]),
}

#[derive(Debug)]
struct KeyShare<'a> {
    group: u16,
    key_exchange: &'a [u8],
}

fn parse_u16_list(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, data) = length_data(be_u16)(input)?;
    let mut items = vec![];
    let mut item_input = data;
    while !item_input.is_empty() {
        let (rest, item) = be_u16(item_input)?;
        items.push(item);
        item_input = rest;
    }
    Ok((input, items))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, client_version) = be_u16(input)?;
    let (input, random) = take(32u8)(input)?;
    let (input, legacy_session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites) = parse_u16_list(input)?;
    let (input, compression_methods) = length_data(be_u8)(input)?;
    let (input, extensions) = opt(parse_extensions)(input)?;
    Ok((
        input,
        ClientHello {
            client_version,
            random,
            legacy_session_id,
            cipher_suites,
            legacy_compression_methods: compression_methods.to_vec(),
            extensions: extensions.unwrap_or_default(),
        },
    ))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (input, ext_data) = length_data(be_u16)(input)?;
    let mut extensions = vec![];
    let mut ext_input = ext_data;
    while !ext_input.is_empty() {
        let (next_input, (ext_type, ext_len)) = tuple((be_u16, be_u16))(ext_input)?;
        let (next_input, ext_body) = take(ext_len)(next_input)?;
        let ext = match ext_type {
            0x002b => map(parse_u16_list, Extension::SupportedVersions)(ext_body),
            0x000d => map(parse_u16_list, Extension::SignatureAlgorithms)(ext_body),
            0x000a => map(parse_u16_list, Extension::SupportedGroups)(ext_body),
            0x0033 => map(parse_key_share, Extension::KeyShare)(ext_body),
            0x0029 => map(length_data(be_u8), Extension::PreSharedKey)(ext_body),
            0x002d => map(length_data(be_u8), |modes: &[u8]| {
                Extension::PskKeyExchangeModes(modes.to_vec())
            })(ext_body),
            0x002a => Ok((next_input, Extension::EarlyData)),
            0x0000 => map(length_data(be_u16), Extension::ServerName)(ext_body),
            _ => Ok((next_input, Extension::Unknown(ext_type, ext_body))),
        }?.1;
        extensions.push(ext);
        ext_input = next_input;
    }
    Ok((input, extensions))
}

fn parse_key_share(input: &[u8]) -> IResult<&[u8], KeyShare> {
    let (input, group) = be_u16(input)?;
    let (input, key_exchange) = length_data(be_u16)(input)?;
    Ok((input, KeyShare { group, key_exchange }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binaryfile>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let data = fs::read(filename).expect("Failed to read file");

    match parse_client_hello(&data) {
        Ok((_remaining, client_hello)) => {
            println!("{:#?}", client_hello);
        }
        Err(e) => {
            eprintln!("Failed to parse ClientHello: {:?}", e);
            process::exit(1);
        }
    }
}