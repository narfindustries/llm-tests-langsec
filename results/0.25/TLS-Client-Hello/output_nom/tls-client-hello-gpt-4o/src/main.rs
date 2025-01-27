use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt},
    multi::{length_data, length_count, many0, many_m_n},
    number::complete::{be_u16, be_u24, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

// TLS Client Hello structure
#[derive(Debug)]
struct ClientHello {
    version: u16,
    random: [u8; 32],
    session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

// Parser for TLS Client Hello
fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, version) = be_u16(input)?;
    let (input, random) = map(take(32usize), |bytes: &[u8]| {
        let mut array = [0u8; 32];
        array.copy_from_slice(bytes);
        array
    })(input)?;
    let (input, session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites) = length_count(be_u16, be_u16)(input)?;
    let (input, compression_methods) = length_count(be_u8, be_u8)(input)?;
    let (input, extensions) = map_opt(length_data(be_u16), |ext_data: &[u8]| {
        many0(parse_extension)(ext_data).ok()
    })(input)?;

    Ok((
        input,
        ClientHello {
            version,
            random,
            session_id: session_id.to_vec(),
            cipher_suites,
            compression_methods,
            extensions,
        },
    ))
}

// Parser for TLS Extension
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

    let file_path = &args[1];
    let mut file = File::open(file_path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => {
            println!("{:#?}", client_hello);
        }
        Err(e) => {
            eprintln!("Failed to parse Client Hello: {:?}", e);
        }
    }
}