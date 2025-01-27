// Complete Rust program using Nom to parse a TLS Client Hello message

use nom::{
    bytes::complete::{tag, take},
    combinator::{map_res, opt},
    error::ErrorKind,
    multi::{length_data, length_value, many0, many1},
    number::complete::{be_u16, be_u24, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct ClientHello {
    version: u16,
    random: Vec<u8>,
    session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_tls_plaintext(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _) = tag([0x16u8])(input)?; // Content Type: Handshake
    let (input, _) = be_u16(input)?; // Version
    let (input, _) = length_data(be_u16)(input)?; // Length Prefixed Record
    parse_handshake(input)
}

fn parse_handshake(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _) = tag([0x01u8])(input)?; // Handshake Type: ClientHello
    let (input, _) = length_value(be_u24, parse_client_hello)(input)?;
    Ok((input, ClientHello {
        version: 0,
        random: vec![],
        session_id: vec![],
        cipher_suites: vec![],
        compression_methods: vec![],
        extensions: vec![],
    }))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, version) = be_u16(input)?;
    let (input, random) = take(32u8)(input)?;
    let (input, session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites) = length_data(be_u16)(input)?;
    let (input, compression_methods) = length_data(be_u8)(input)?;
    let (input, extensions) = opt(length_value(be_u16, many0(parse_extension)))(input)?;

    Ok((input, ClientHello {
        version,
        random: random.to_vec(),
        session_id: session_id.to_vec(),
        cipher_suites: cipher_suites.chunks(2).map(|s| u16::from_be_bytes([s[0], s[1]])).collect(),
        compression_methods: compression_methods.to_vec(),
        extensions: extensions.unwrap_or_default(),
    }))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_data) = length_data(be_u16)(input)?;
    Ok((input, Extension {
        extension_type,
        extension_data: extension_data.to_vec(),
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tls_plaintext(&buffer) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Failed to parse TLS ClientHello: {:?}", e),
    }

    Ok(())
}