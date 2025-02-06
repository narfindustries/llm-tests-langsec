use nom::bytes::complete::{take};
use nom::number::complete::{be_u8, be_u16};
use nom::multi::{length_data, length_count};
use nom::sequence::tuple;
use nom::combinator::{map, success};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub struct ClientHello {
    legacy_version: u16,
    random: [u8; 32],
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
pub struct Extension {
    extension_type: u16,
    data: Vec<u8>,
}

fn parse_random(input: &[u8]) -> IResult<&[u8], [u8; 32]> {
    let (input, random_bytes) = take(32usize)(input)?;
    let mut random = [0u8; 32];
    random.copy_from_slice(random_bytes);
    Ok((input, random))
}

fn parse_legacy_session_id(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(length_data(be_u8), |data: &[u8]| data.to_vec())(input)
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, total_length) = be_u16(input)?;
    let num_suites = total_length / 2;
    length_count(success(num_suites), be_u16)(input)
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(length_data(be_u8), |data: &[u8]| data.to_vec())(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, (extension_type, data)) = tuple((
        be_u16,
        map(length_data(be_u16), |d: &[u8]| d.to_vec()),
    ))(input)?;

    Ok((input, Extension {
        extension_type,
        data,
    }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (input, extensions_data) = length_data(be_u16)(input)?;
    let mut extensions = Vec::new();
    let mut remaining = extensions_data;

    while !remaining.is_empty() {
        let (new_remaining, extension) = parse_extension(remaining)?;
        extensions.push(extension);
        remaining = new_remaining;
    }

    Ok((input, extensions))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, (
        legacy_version,
        random,
        legacy_session_id,
        cipher_suites,
        compression_methods,
        extensions
    )) = tuple((
        be_u16,
        parse_random,
        parse_legacy_session_id,
        parse_cipher_suites,
        parse_compression_methods,
        parse_extensions,
    ))(input)?;

    Ok((input, ClientHello {
        legacy_version,
        random,
        legacy_session_id,
        cipher_suites,
        compression_methods,
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
        Ok((remaining, client_hello)) => {
            println!("Parsed ClientHello: {:#?}", client_hello);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse ClientHello: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}