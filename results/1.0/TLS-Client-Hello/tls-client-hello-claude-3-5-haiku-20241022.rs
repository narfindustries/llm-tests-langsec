use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, length_count, length_data},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TlsClientHello {
    version: u16,
    random: [u8; 32],
    session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Option<Vec<Extension>>,
}

#[derive(Debug)]
struct Extension {
    type_: u16,
    data: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], TlsClientHello> {
    map(
        tuple((
            tag(&[0x16]),  // Handshake type
            tag(&[0x03, 0x01]),  // TLS version
            be_u16,  // Length
            tag(&[0x01]),  // ClientHello handshake type
            be_u16,  // Length
            be_u16,  // Client version
            take(32usize),  // Random
            length_data(be_u8),  // Session ID
            length_count(be_u16, be_u16),  // Cipher suites
            length_count(be_u8, be_u8),  // Compression methods
            opt(length_count(be_u16, parse_extension))  // Extensions
        )),
        |(_, _, _, _, _, version, random, session_id, cipher_suites, compression_methods, extensions)| TlsClientHello {
            version,
            random: random.try_into().unwrap(),
            session_id,
            cipher_suites,
            compression_methods,
            extensions,
        }
    )(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    map(
        tuple((
            be_u16,  // Extension type
            length_data(be_u16)  // Extension data
        )),
        |(type_, data)| Extension {
            type_,
            data: data.to_vec(),
        }
    )(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
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