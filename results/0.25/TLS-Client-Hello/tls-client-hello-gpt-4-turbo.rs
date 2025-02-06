use nom::{
    bytes::complete::{take, take_while1},
    combinator::{map, map_res},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct ClientHello {
    legacy_version: u16,
    random: Vec<u8>,
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    legacy_compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
enum Extension {
    ServerName(String),
    SupportedGroups(Vec<u16>),
    SignatureAlgorithms(Vec<u16>),
    SupportedVersions(Vec<u16>),
    PskModes(Vec<u8>),
    KeyShare(Vec<KeyShareEntry>),
    Unknown(u16, Vec<u8>),
}

#[derive(Debug)]
struct KeyShareEntry {
    group: u16,
    key_exchange: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, legacy_session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites) = preceded(be_u16, many0(be_u16))(input)?;
    let (input, legacy_compression_methods) = length_data(be_u8)(input)?;
    let (input, extensions) = preceded(be_u16, many0(parse_extension))(input)?;

    Ok((
        input,
        ClientHello {
            legacy_version,
            random: random.to_vec(),
            legacy_session_id: legacy_session_id.to_vec(),
            cipher_suites,
            legacy_compression_methods: legacy_compression_methods.to_vec(),
            extensions,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_data) = length_data(be_u16)(input)?;

    match extension_type {
        0 => {
            let (rest, server_name) = map_res(take_while1(|b: u8| b != 0), std::str::from_utf8)(extension_data)?;
            Ok((input, Extension::ServerName(server_name.to_string())))
        }
        10 => {
            let (rest, groups) = many0(be_u16)(extension_data)?;
            Ok((input, Extension::SupportedGroups(groups)))
        }
        13 => {
            let (rest, algorithms) = many0(be_u16)(extension_data)?;
            Ok((input, Extension::SignatureAlgorithms(algorithms)))
        }
        43 => {
            let (rest, versions) = many0(be_u16)(extension_data)?;
            Ok((input, Extension::SupportedVersions(versions)))
        }
        45 => {
            let (rest, modes) = many0(be_u8)(extension_data)?;
            Ok((input, Extension::PskModes(modes)))
        }
        51 => {
            let (rest, key_shares) = many0(parse_key_share_entry)(extension_data)?;
            Ok((input, Extension::KeyShare(key_shares)))
        }
        _ => Ok((input, Extension::Unknown(extension_type, extension_data.to_vec()))),
    }
}

fn parse_key_share_entry(input: &[u8]) -> IResult<&[u8], KeyShareEntry> {
    let (input, group) = be_u16(input)?;
    let (input, key_exchange) = length_data(be_u16)(input)?;
    Ok((
        input,
        KeyShareEntry {
            group,
            key_exchange: key_exchange.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Usage: tls_parser <file_path>"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => {
            println!("{:#?}", client_hello);
        }
        Err(e) => {
            println!("Failed to parse ClientHello: {:?}", e);
        }
    }

    Ok(())
}