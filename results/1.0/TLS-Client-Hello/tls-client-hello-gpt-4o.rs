use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    combinator::{map, map_opt},
    number::complete::{be_u8, be_u16, be_u24},
    multi::length_count,
};

#[derive(Debug)]
struct ClientHello<'a> {
    legacy_version: u16,
    random: &'a [u8],
    session_id: &'a [u8],
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension<'a>>,
}

#[derive(Debug)]
struct Extension<'a> {
    extension_type: u16,
    extension_data: &'a [u8],
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, session_id_len) = be_u8(input)?;
    let (input, session_id) = take(session_id_len as usize)(input)?;
    let (input, cipher_suites) = length_count(be_u16, be_u16)(input)?;
    let (input, compression_methods) = length_count(be_u8, be_u8)(input)?;
    let (input, extensions) = length_count(be_u16, parse_extension)(input)?;

    Ok((input, ClientHello {
        legacy_version,
        random,
        session_id,
        cipher_suites,
        compression_methods,
        extensions,
    }))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, extension_data) = take(length)(input)?;

    Ok((input, Extension {
        extension_type,
        extension_data,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path_to_binary_file>", args[0]);
        std::process::exit(1);
    }
    let file_path = &args[1];

    let mut file = File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => {
            println!("{:?}", client_hello);
        }
        Err(e) => {
            eprintln!("Failed to parse ClientHello: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}