use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::{length_data, length_count, many0},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct ClientHello {
    legacy_version: u16,
    random: Vec<u8>,
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

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, legacy_session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites) = length_count(be_u16, be_u16)(input)?;
    let (input, legacy_compression_methods) = length_data(be_u8)(input)?;
    let (input, extensions) = length_count(be_u16, parse_extension)(input)?;

    Ok((
        input,
        ClientHello {
            legacy_version,
            random: random.to_vec(),
            legacy_session_id: legacy_session_id.to_vec(),
            cipher_suites,
            legacy_compression_methods: legacy_compression_methods.to_vec(),
            extensions,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_data) = length_data(be_u16)(input)?;

    Ok((
        input,
        Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Error parsing ClientHello: {:?}", e),
    }
}