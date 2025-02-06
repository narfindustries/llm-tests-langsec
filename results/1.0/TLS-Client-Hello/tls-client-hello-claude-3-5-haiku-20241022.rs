use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::{count, many_m_n},
    number::complete::{be_u8, be_u16, be_u24},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct CipherSuite(u16);

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ClientHello {
    legacy_version: u16,
    random: [u8; 32],
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<CipherSuite>,
    legacy_compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

fn parse_random(input: &[u8]) -> IResult<&[u8], [u8; 32]> {
    let (input, random_bytes) = take(32usize)(input)?;
    Ok((input, random_bytes.try_into().unwrap()))
}

fn parse_legacy_session_id(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u8(input)?;
    let (input, session_id) = take(length as usize)(input)?;
    Ok((input, session_id.to_vec()))
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<CipherSuite>> {
    let (input, length) = be_u16(input)?;
    count(map(be_u16, CipherSuite), (length / 2) as usize)(input)
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u8(input)?;
    let (input, methods) = take(length as usize)(input)?;
    Ok((input, methods.to_vec()))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length as usize)(input)?;
    Ok((input, Extension {
        extension_type,
        data: data.to_vec(),
    }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (input, _length) = be_u16(input)?;
    many_m_n(1, 10, parse_extension)(input)
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _handshake_type) = tag(&[0x01])(input)?;
    let (input, _length) = be_u24(input)?;
    
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = parse_random(input)?;
    let (input, legacy_session_id) = parse_legacy_session_id(input)?;
    let (input, cipher_suites) = parse_cipher_suites(input)?;
    let (input, legacy_compression_methods) = parse_compression_methods(input)?;
    let (input, extensions) = parse_extensions(input)?;

    Ok((input, ClientHello {
        legacy_version,
        random,
        legacy_session_id,
        cipher_suites,
        legacy_compression_methods,
        extensions,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => {
            println!("Parsed ClientHello: {:?}", client_hello);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}