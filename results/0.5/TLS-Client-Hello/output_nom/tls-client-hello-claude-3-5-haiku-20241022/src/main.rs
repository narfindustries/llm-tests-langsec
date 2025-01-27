use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, length_count, length_data},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
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
    extensions: Option<Vec<TlsExtension>>,
}

#[derive(Debug)]
struct TlsExtension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], TlsClientHello> {
    let (input, _) = tag(&[0x16])(input)?; // Handshake record type
    let (input, _) = tag(&[0x03, 0x01])(input)?; // TLS version
    let (input, record_length) = be_u16(input)?;

    let (input, handshake_type) = tag(&[0x01])(input)?; // ClientHello
    let (input, handshake_length) = take(3usize)(input)?;

    let (input, version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, session_id_length) = be_u8(input)?;
    let (input, session_id) = take(session_id_length as usize)(input)?;

    let (input, cipher_suites_length) = be_u16(input)?;
    let (input, cipher_suites) = count(be_u16, cipher_suites_length as usize / 2)(input)?;

    let (input, compression_methods_length) = be_u8(input)?;
    let (input, compression_methods) = take(compression_methods_length as usize)(input)?;

    let (input, extensions) = opt(parse_extensions)(input)?;

    Ok((input, TlsClientHello {
        version,
        random: random.try_into().unwrap(),
        session_id: session_id.to_vec(),
        cipher_suites,
        compression_methods: compression_methods.to_vec(),
        extensions,
    }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<TlsExtension>> {
    let (input, extensions_length) = be_u16(input)?;
    length_count(
        map(tag(&[0u8]), |_| extensions_length),
        parse_extension
    )(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], TlsExtension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_data_length) = be_u16(input)?;
    let (input, extension_data) = take(extension_data_length as usize)(input)?;

    Ok((input, TlsExtension {
        extension_type,
        extension_data: extension_data.to_vec(),
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input_file = &args[1];
    let input_data = fs::read(input_file).expect("Failed to read input file");

    match parse_client_hello(&input_data) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Parsing error: {:?}", e),
    }
}