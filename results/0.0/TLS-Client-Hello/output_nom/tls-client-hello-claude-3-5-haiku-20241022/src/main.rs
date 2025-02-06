use nom::{
    bytes::complete::{take, tag},
    combinator::map,
    multi::{length_count, length_data},
    number::complete::{be_u8, be_u16, be_u24},
    sequence::{tuple, preceded},
    IResult,
    error::ErrorKind,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct CipherSuite(u16);

#[derive(Debug)]
struct Extension {
    ext_type: u16,
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
    let (input, random) = take(32usize)(input)?;
    Ok((input, random.try_into().unwrap()))
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<CipherSuite>> {
    length_count(
        be_u16,
        map(be_u16, |suite| CipherSuite(suite))
    )(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    map(
        tuple((
            be_u16,
            length_data(be_u16::<&[u8], nom::error::Error<&[u8]>>)
        )),
        |(ext_type, data)| Extension {
            ext_type,
            data: data.to_vec(),
        }
    )(input)
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    preceded(
        tag(&[0x01]), // Handshake type: ClientHello
        map(
            tuple((
                be_u24,               // Length
                be_u16,               // Legacy version
                parse_random,          // Random
                length_count(be_u8, be_u8), // Legacy session ID
                parse_cipher_suites,   // Cipher suites
                length_count(be_u8, be_u8), // Legacy compression methods
                length_count(be_u16, parse_extension) // Extensions
            )),
            |(_, legacy_version, random, legacy_session_id, 
              cipher_suites, legacy_compression_methods, extensions)| 
            ClientHello {
                legacy_version,
                random,
                legacy_session_id,
                cipher_suites,
                legacy_compression_methods,
                extensions,
            }
        )
    )(input)
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
            std::process::exit(1)
        }
    }
}