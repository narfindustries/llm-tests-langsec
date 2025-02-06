use nom::{
    bytes::complete::take,
    combinator::map,
    multi::length_data,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ProtocolVersion(u8, u8);

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
    legacy_compression_methods: Vec<CompressionMethod>,
    extensions: Vec<Extension>,
}

fn parse_protocol_version(input: &[u8]) -> IResult<&[u8], ProtocolVersion> {
    map(tuple((be_u8, be_u8)), |(major, minor)| {
        ProtocolVersion(major, minor)
    })(input)
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Random> {
    map(take(32usize), |bytes: &[u8]| {
        let mut random = [0u8; 32];
        random.copy_from_slice(bytes);
        Random(random)
    })(input)
}

fn parse_legacy_session_id(input: &[u8]) -> IResult<&[u8], LegacySessionID> {
    map(length_data(be_u8), |bytes: &[u8]| {
        LegacySessionID(bytes.to_vec())
    })(input)
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<CipherSuite>> {
    map(length_data(be_u16), |bytes: &[u8]| {
        bytes.chunks(2).map(|chunk| CipherSuite(u16::from_be_bytes([chunk[0], chunk[1]]))).collect()
    })(input)
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<CompressionMethod>> {
    map(length_data(be_u8), |bytes: &[u8]| {
        bytes.iter().map(|&b| CompressionMethod(b)).collect()
    })(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    map(tuple((be_u16::<&[u8], nom::error::Error<&[u8]>>, length_data(be_u16))), |(extension_type, extension_data)| {
        Extension { extension_type, extension_data: extension_data.to_vec() }
    })(input)
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (input, extensions_data) = length_data(be_u16)(input)?;
    let mut extensions = Vec::new();
    let mut remaining = extensions_data;
    while !remaining.is_empty() {
        let (rest, extension) = parse_extension(remaining)?;
        extensions.push(extension);
        remaining = rest;
    }
    Ok((input, extensions))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, protocol_version) = parse_protocol_version(input)?;
    let (input, random) = parse_random(input)?;
    let (input, legacy_session_id) = parse_legacy_session_id(input)?;
    let (input, cipher_suites) = parse_cipher_suites(input)?;
    let (input, legacy_compression_methods) = parse_compression_methods(input)?;
    let (input, extensions) = parse_extensions(input)?;

    Ok((
        input,
        ClientHello {
            protocol_version,
            random,
            legacy_session_id,
            cipher_suites,
            legacy_compression_methods,
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

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Failed to parse ClientHello: {:?}", e),
    }
}