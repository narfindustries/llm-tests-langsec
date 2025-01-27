use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, length_count, length_data},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{pair, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ClientHello {
    version: u16,
    random: [u8; 32],
    session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Option<Vec<Extension>>,
}

#[derive(Debug)]
struct Extension {
    ext_type: u16,
    ext_data: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _) = tag(&[0x16])(input)?; // Handshake type
    let (input, _) = tag(&[0x03, 0x01])(input)?; // Protocol version
    let (input, _length) = be_u16(input)?;

    let (input, _handshake_type) = tag(&[0x01])(input)?; // ClientHello
    let (input, _handshake_length) = take(3usize)(input)?;

    let (input, version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites) = length_count(be_u16, be_u16)(input)?;
    let (input, compression_methods) = length_count(be_u8, be_u8)(input)?;

    let (input, extensions) = opt(length_count(be_u16, parse_extension))(input)?;

    Ok((input, ClientHello {
        version,
        random: random.try_into().unwrap(),
        session_id: session_id.to_vec(),
        cipher_suites,
        compression_methods,
        extensions,
    }))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, ext_type) = be_u16(input)?;
    let (input, ext_data) = length_data(be_u16)(input)?;

    Ok((input, Extension {
        ext_type,
        ext_data: ext_data.to_vec(),
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
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}