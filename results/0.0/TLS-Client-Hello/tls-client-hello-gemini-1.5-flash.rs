use nom::{
    bytes::complete::{tag, take, take_until},
    combinator::{map, map_res, opt},
    error::ErrorKind,
    multi::length_count,
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs;
use std::path::Path;

#[derive(Debug)]
struct ClientHello {
    client_version: u16,
    random: [u8; 32],
    legacy_session_id: Option<Vec<u8>>,
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
    let (rem, client_version) = be_u16(input)?;
    let (rem, random) = take(32usize)(rem)?;
    let (rem, legacy_session_id) = preceded(be_u8, opt(take_until("\0")))(rem)?;
    let legacy_session_id = legacy_session_id.map(|s| s[..s.len() - 1].to_vec());
    let (rem, cipher_suites) = length_count(be_u16, be_u16)(rem)?;
    let (rem, legacy_compression_methods) = length_count(be_u8, be_u8)(rem)?;
    let (rem, extensions) = length_count(be_u16, parse_extension)(rem)?;

    Ok((
        rem,
        ClientHello {
            client_version,
            random: random.try_into().unwrap(),
            legacy_session_id,
            cipher_suites,
            legacy_compression_methods,
            extensions,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (rem, extension_type) = be_u16(input)?;
    let (rem, extension_data_len) = be_u16(rem)?;
    let (rem, extension_data) = take(extension_data_len as usize)(rem)?;
    Ok((
        rem,
        Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    ))
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let contents = fs::read(path)?;

    match parse_client_hello(&contents) {
        Ok((rem, client_hello)) => {
            println!("Parsed Client Hello:\n{:?}", client_hello);
            if !rem.is_empty() {
                println!("Remaining bytes: {:?}", rem);
            }
        }
        Err(e) => {
            println!("Error parsing Client Hello: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}

