use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::length_count,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct ClientHello {
    legacy_version: u16,
    random: [u8; 32],
    session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    data: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let random: [u8; 32] = random.try_into().expect("slice with incorrect length");
    let (input, session_id) = length_count(be_u8, be_u8)(input)?;
    let (input, cipher_suites) = length_count(be_u16, be_u16)(input)?;
    let (input, compression_methods) = length_count(be_u8, be_u8)(input)?;
    let (input, extensions) = length_count(be_u16, parse_extension)(input)?;

    Ok((
        input,
        ClientHello {
            legacy_version,
            random,
            session_id,
            cipher_suites,
            compression_methods,
            extensions,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, data) = length_count(be_u16, be_u8)(input)?;
    Ok((
        input,
        Extension {
            extension_type,
            data,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Could not open file");

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Failed to parse ClientHello: {:?}", e),
    }
}