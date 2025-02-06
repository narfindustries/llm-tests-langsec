use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::length_count,
    number::complete::{be_u16, be_u24, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
struct ProtocolVersion(u16);

#[derive(Debug)]
struct Random([u8; 32]);

#[derive(Debug)]
struct LegacySessionID(Vec<u8>);

#[derive(Debug)]
struct CipherSuite(u16);

#[derive(Debug)]
struct CompressionMethod(u8);

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

#[derive(Debug)]
struct ClientHello {
    protocol_version: ProtocolVersion,
    random: Random,
    legacy_session_id: LegacySessionID,
    cipher_suites: Vec<CipherSuite>,
    compression_methods: Vec<CompressionMethod>,
    extensions: Vec<Extension>,
}

fn parse_protocol_version(input: &[u8]) -> IResult<&[u8], ProtocolVersion> {
    map(be_u16, ProtocolVersion)(input)
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Random> {
    map(take(32usize), |bytes: &[u8]| {
        let mut arr = [0u8; 32];
        arr.copy_from_slice(bytes);
        Random(arr)
    })(input)
}

fn parse_legacy_session_id(input: &[u8]) -> IResult<&[u8], LegacySessionID> {
    map(length_count(be_u8, be_u8), LegacySessionID)(input)
}

fn parse_cipher_suite(input: &[u8]) -> IResult<&[u8], CipherSuite> {
    map(be_u16, CipherSuite)(input)
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<CipherSuite>> {
    length_count(be_u16, parse_cipher_suite)(input)
}

fn parse_compression_method(input: &[u8]) -> IResult<&[u8], CompressionMethod> {
    map(be_u8, CompressionMethod)(input)
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<CompressionMethod>> {
    length_count(be_u8, parse_compression_method)(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_data) = length_count(be_u16, be_u8)(input)?;
    Ok((
        input,
        Extension {
            extension_type,
            extension_data,
        },
    ))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    length_count(be_u16, parse_extension)(input)
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _) = tag([0x01])(input)?; // HandshakeType::ClientHello
    let (input, length) = be_u24(input)?;
    let (input, protocol_version) = parse_protocol_version(input)?;
    let (input, random) = parse_random(input)?;
    let (input, legacy_session_id) = parse_legacy_session_id(input)?;
    let (input, cipher_suites) = parse_cipher_suites(input)?;
    let (input, compression_methods) = parse_compression_methods(input)?;
    let (input, extensions) = parse_extensions(input)?;
    Ok((
        input,
        ClientHello {
            protocol_version,
            random,
            legacy_session_id,
            cipher_suites,
            compression_methods,
            extensions,
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
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_client_hello(&data) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Failed to parse ClientHello: {:?}", e),
    }
}