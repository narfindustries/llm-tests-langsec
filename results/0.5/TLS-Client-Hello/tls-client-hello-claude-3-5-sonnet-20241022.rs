use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ClientHello {
    legacy_version: u16,
    random: Vec<u8>,
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    data: Vec<u8>,
}

fn parse_legacy_version(input: &[u8]) -> IResult<&[u8], u16> {
    verify(be_u16, |&x| x == 0x0303)(input)
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take(32usize), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_legacy_session_id(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(length_data(be_u8), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length)(input)?;
    let mut cipher_suites = Vec::new();
    let mut slice = data;
    while !slice.is_empty() {
        let (remaining, suite) = be_u16(slice)?;
        cipher_suites.push(suite);
        slice = remaining;
    }
    Ok((input, cipher_suites))
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(length_data(be_u8), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, (extension_type, length)) = tuple((be_u16, be_u16))(input)?;
    let (input, data) = map(take(length), |bytes: &[u8]| bytes.to_vec())(input)?;
    Ok((input, Extension {
        extension_type,
        data,
    }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length)(input)?;
    let (_, extensions) = many0(parse_extension)(data)?;
    Ok((input, extensions))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _) = tag([0x16])(input)?; // Content Type: Handshake
    let (input, _) = tag([0x03, 0x03])(input)?; // Legacy Record Layer Version
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length)(input)?;
    
    let (_, (
        _handshake_type,
        _length,
        legacy_version,
        random,
        legacy_session_id,
        cipher_suites,
        compression_methods,
        extensions
    )) = tuple((
        verify(be_u8, |&x| x == 1), // Handshake Type: ClientHello
        be_u24,
        parse_legacy_version,
        parse_random,
        parse_legacy_session_id,
        parse_cipher_suites,
        parse_compression_methods,
        parse_extensions,
    ))(data)?;

    Ok((input, ClientHello {
        legacy_version,
        random,
        legacy_session_id,
        cipher_suites,
        compression_methods,
        extensions,
    }))
}

fn be_u24(input: &[u8]) -> IResult<&[u8], u32> {
    let (input, bytes) = take(3usize)(input)?;
    Ok((input, ((bytes[0] as u32) << 16) | ((bytes[1] as u32) << 8) | (bytes[2] as u32)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tls_client_hello_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_client_hello(&buffer) {
        Ok((remaining, client_hello)) => {
            println!("Successfully parsed ClientHello:");
            println!("{:#?}", client_hello);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse ClientHello: {:?}", e);
        }
    }
}