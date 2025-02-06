use nom::{
    bytes::complete::take,
    combinator::{map_parser, map_res},
    multi::{length_data, length_value},
    number::complete::be_u16,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct ClientHello {
    legacy_version: u16,
    random: [u8; 32],
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    legacy_compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
enum Extension {
    SupportedVersions(Vec<u16>),
    KeyShare(Vec<u8>),
    Other(Vec<u8>),
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, legacy_session_id) = length_value(take(1usize), take)(input)?;
    let (input, cipher_suites) = length_value(be_u16, map_parser(take, |data: &[u8]| {
        map_res(data.chunks(2), |chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
    }))(input)?;
    let (input, legacy_compression_methods) = length_value(take(1usize), take)(input)?;
    let (input, extensions) = parse_extensions(input)?;

    let client_hello = ClientHello {
        legacy_version,
        random: random.try_into().unwrap(),
        legacy_session_id: legacy_session_id.to_vec(),
        cipher_suites,
        legacy_compression_methods: legacy_compression_methods.to_vec(),
        extensions,
    };

    Ok((input, client_hello))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (input, extensions_data) = length_value(be_u16, take)(input)?;
    let mut extensions = Vec::new();
    let mut remaining = extensions_data;

    while !remaining.is_empty() {
        let (input, extension_type) = be_u16(remaining)?;
        let (input, extension_data) = length_value(be_u16, take)(input)?;

        let extension = match extension_type {
            0x002b => Extension::SupportedVersions(extension_data.chunks(2).map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]])).collect()),
            0x0033 => Extension::KeyShare(extension_data.to_vec()),
            _ => Extension::Other(extension_data.to_vec()),
        };

        extensions.push(extension);
        remaining = input;
    }

    Ok((input, extensions))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Failed to parse ClientHello: {:?}", e),
    }
}