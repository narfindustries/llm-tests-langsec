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
struct TlsClientHello {
    legacy_version: u16,
    random: [u8; 32],
    session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<TlsExtension>,
}

#[derive(Debug)]
struct TlsExtension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], TlsClientHello> {
    map(
        preceded(
            tag(&[0x16, 0x03, 0x03]),
            tuple((
                be_u24,
                be_u16,
                take(32usize),
                length_data(be_u8),
                length_count(be_u16, be_u16),
                length_count(be_u8, be_u8),
                length_count(be_u16, parse_extension)
            ))
        ),
        |(_, legacy_version, random, session_id, cipher_suites, compression_methods, extensions)| {
            TlsClientHello {
                legacy_version,
                random: random.try_into().unwrap(),
                session_id: session_id.to_vec(),
                cipher_suites,
                compression_methods,
                extensions,
            }
        }
    )(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], TlsExtension> {
    map(
        tuple((
            be_u16::<&[u8], nom::error::Error<&[u8]>>,
            length_data(be_u16::<&[u8], nom::error::Error<&[u8]>>)
        )),
        |(extension_type, extension_data)| {
            TlsExtension {
                extension_type,
                extension_data: extension_data.to_vec(),
            }
        }
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
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
            println!("Parsed TLS Client Hello: {:?}", client_hello);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}