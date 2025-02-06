use nom::{
    bytes::complete::take,
    combinator::map,
    multi::{length_count, length_data},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Random {
    gmt_unix_time: u32,
    random_bytes: Vec<u8>,
}

#[derive(Debug)]
struct ClientHello {
    legacy_version: u16,
    random: Random,
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    legacy_compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Random> {
    map(
        tuple((be_u32, take(28usize))),
        |(gmt_unix_time, random_bytes)| Random {
            gmt_unix_time,
            random_bytes: random_bytes.to_vec(),
        }
    )(input)
}

fn parse_legacy_session_id(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(
        length_data(be_u8),
        |data: &[u8]| data.to_vec()
    )(input)
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    length_count(be_u16, be_u16)(input)
}

fn parse_legacy_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    length_count(be_u8, be_u8)(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    map(
        tuple((be_u16, length_data(be_u16))),
        |(extension_type, extension_data)| Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        }
    )(input)
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    length_count(be_u16, parse_extension)(input)
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    map(
        tuple((
            be_u16,
            parse_random,
            parse_legacy_session_id,
            parse_cipher_suites,
            parse_legacy_compression_methods,
            parse_extensions
        )),
        |(legacy_version, random, legacy_session_id, cipher_suites, 
          legacy_compression_methods, extensions)| ClientHello {
            legacy_version,
            random,
            legacy_session_id,
            cipher_suites,
            legacy_compression_methods,
            extensions,
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