use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::length_data,
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct ClientHello {
    client_version: [u8; 2],
    random: [u8; 32],
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    legacy_compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, client_version) = take(2usize)(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, legacy_session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites) = length_data(be_u16)(input)?;
    let (input, legacy_compression_methods) = length_data(be_u8)(input)?;
    let (input, extensions) = length_data(be_u16)(input)?;

    let cipher_suites: Vec<u16> = cipher_suites
        .chunks(2)
        .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
        .collect();

    let legacy_compression_methods: Vec<u8> = legacy_compression_methods.to_vec();

    let mut ext_input = extensions;
    let mut ext_vec = Vec::new();
    while !ext_input.is_empty() {
        let (rest, ext) = parse_extension(ext_input)?;
        ext_vec.push(ext);
        ext_input = rest;
    }

    Ok((
        input,
        ClientHello {
            client_version: [client_version[0], client_version[1]],
            random: random.try_into().unwrap(),
            legacy_session_id: legacy_session_id.to_vec(),
            cipher_suites,
            legacy_compression_methods,
            extensions: ext_vec,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_data) = length_data(be_u16)(input)?;
    Ok((
        input,
        Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => {
            println!("{:#?}", client_hello);
        }
        Err(e) => {
            eprintln!("Failed to parse ClientHello: {:?}", e);
        }
    }

    Ok(())
}