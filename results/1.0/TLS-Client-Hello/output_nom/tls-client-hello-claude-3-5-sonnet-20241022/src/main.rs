use nom::bytes::complete::{tag, take};
use nom::multi::{length_data, many0};
use nom::number::complete::{be_u16, be_u24, be_u32, be_u8};
use nom::sequence::tuple;
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub struct ClientHello {
    version: (u8, u8),
    random: ClientRandom,
    session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
pub struct ClientRandom {
    gmt_unix_time: u32,
    random_bytes: [u8; 28],
}

#[derive(Debug)]
pub struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_client_random(input: &[u8]) -> IResult<&[u8], ClientRandom> {
    let (input, gmt_unix_time) = be_u32(input)?;
    let (input, random_bytes) = take(28usize)(input)?;
    let mut random_array = [0u8; 28];
    random_array.copy_from_slice(random_bytes);
    Ok((
        input,
        ClientRandom {
            gmt_unix_time,
            random_bytes: random_array,
        },
    ))
}

fn parse_session_id(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u8(input)?;
    let (input, session_id) = take(length as usize)(input)?;
    Ok((input, session_id.to_vec()))
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, length) = be_u16(input)?;
    let mut cipher_suites = Vec::new();
    let (input, cipher_data) = take(length as usize)(input)?;
    let mut remaining = cipher_data;
    while !remaining.is_empty() {
        let (rest, cipher) = be_u16(remaining)?;
        cipher_suites.push(cipher);
        remaining = rest;
    }
    Ok((input, cipher_suites))
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u8(input)?;
    let (input, methods) = take(length as usize)(input)?;
    Ok((input, methods.to_vec()))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_length) = be_u16(input)?;
    let (input, extension_data) = take(extension_length as usize)(input)?;
    Ok((
        input,
        Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    ))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (input, total_length) = be_u16(input)?;
    let (input, extension_data) = take(total_length as usize)(input)?;
    let (_, extensions) = many0(parse_extension)(extension_data)?;
    Ok((input, extensions))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _) = tag(&[0x16])(input)?; // Handshake type
    let (input, _) = tag(&[0x03, 0x03])(input)?; // TLS version
    let (input, length) = be_u24(input)?;
    let (input, _) = tag(&[0x01])(input)?; // Client Hello type
    let (input, _) = be_u24(input)?; // Length
    let (input, version) = tuple((be_u8, be_u8))(input)?;
    let (input, random) = parse_client_random(input)?;
    let (input, session_id) = parse_session_id(input)?;
    let (input, cipher_suites) = parse_cipher_suites(input)?;
    let (input, compression_methods) = parse_compression_methods(input)?;
    let (input, extensions) = parse_extensions(input)?;

    Ok((
        input,
        ClientHello {
            version,
            random,
            session_id,
            cipher_suites,
            compression_methods,
            extensions,
        },
    ))
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
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Error parsing ClientHello: {}", e),
    }

    Ok(())
}