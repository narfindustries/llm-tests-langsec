use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, length_count, length_data},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::env;
use std::fs;

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
    ext_type: u16,
    ext_data: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], TlsClientHello> {
    map(
        tuple((
            tag(&[0x16]),  // Handshake type
            tag(&[0x03, 0x01]),  // TLS version
            be_u16,  // Length
            tag(&[0x01]),  // Handshake type (ClientHello)
            be_u16,  // Handshake length
            be_u16,  // TLS version
            take(32usize),  // Random
            length_data(be_u8),  // Session ID
            length_count(be_u16, be_u16),  // Cipher suites
            length_count(be_u8, be_u8),  // Compression methods
            opt(length_count(be_u16, parse_extension)),  // Extensions
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
        pair(be_u16, length_data(be_u16)),
        |(ext_type, ext_data)| Extension {
            ext_type,
            ext_data: ext_data.to_vec(),
        }
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read(&args[1]).expect("Failed to read file");
    match parse_client_hello(&input) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Parsing error: {:?}", e),
    }
}