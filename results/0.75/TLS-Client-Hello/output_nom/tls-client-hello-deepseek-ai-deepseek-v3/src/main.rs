use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
struct ClientHello {
    legacy_version: u16,
    random: [u8; 32],
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    legacy_compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
enum Extension {
    SupportedVersions(Vec<u16>),
    KeyShare(Vec<KeyShareEntry>),
    SignatureAlgorithms(Vec<u16>),
    ServerName(String),
    Other { extension_type: u16, data: Vec<u8> },
}

#[derive(Debug)]
struct KeyShareEntry {
    group: u16,
    key_exchange: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, session_id_len) = be_u8(input)?;
    let (input, legacy_session_id) = take(session_id_len)(input)?;
    let (input, cipher_suites_len) = be_u16(input)?;
    let (input, cipher_suites) = take(cipher_suites_len)(input)?;
    let (input, compression_methods_len) = be_u8(input)?;
    let (input, legacy_compression_methods) = take(compression_methods_len)(input)?;
    let (input, extensions_len) = be_u16(input)?;
    let (input, extensions) = parse_extensions(extensions_len)(input)?;

    let client_hello = ClientHello {
        legacy_version,
        random: random.try_into().unwrap(),
        legacy_session_id: legacy_session_id.to_vec(),
        cipher_suites: cipher_suites
            .chunks_exact(2)
            .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
            .collect(),
        legacy_compression_methods: legacy_compression_methods.to_vec(),
        extensions,
    };

    Ok((input, client_hello))
}

fn parse_extensions(len: u16) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<Extension>> {
    move |input: &[u8]| {
        let (input, extensions_data) = take(len)(input)?;
        let mut extensions = Vec::new();
        let mut remaining = extensions_data;

        while !remaining.is_empty() {
            let (rest, extension) = parse_extension(remaining)?;
            extensions.push(extension);
            remaining = rest;
        }

        Ok((input, extensions))
    }
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, (extension_type, len)) = tuple((be_u16, be_u16))(input)?;
    let (input, data) = take(len)(input)?;

    let extension = match extension_type {
        0x0000 => {
            let (_, server_name) = parse_server_name(data)?;
            Extension::ServerName(server_name)
        }
        0x000D => {
            let (_, algorithms) = parse_signature_algorithms(data)?;
            Extension::SignatureAlgorithms(algorithms)
        }
        0x002B => {
            let (_, versions) = parse_supported_versions(data)?;
            Extension::SupportedVersions(versions)
        }
        0x0033 => {
            let (_, key_share) = parse_key_share(data)?;
            Extension::KeyShare(key_share)
        }
        _ => Extension::Other {
            extension_type,
            data: data.to_vec(),
        },
    };

    Ok((input, extension))
}

fn parse_server_name(input: &[u8]) -> IResult<&[u8], String> {
    let (input, (name_type, len)) = tuple((be_u8, be_u16))(input)?;
    let (input, name) = take(len)(input)?;

    if name_type != 0x00 {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag)));
    }

    let server_name = String::from_utf8(name.to_vec()).unwrap();
    Ok((input, server_name))
}

fn parse_signature_algorithms(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, len) = be_u16(input)?;
    let (input, data) = take(len)(input)?;
    let algorithms = data
        .chunks_exact(2)
        .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
        .collect();
    Ok((input, algorithms))
}

fn parse_supported_versions(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, len) = be_u8(input)?;
    let (input, data) = take(len)(input)?;
    let versions = data
        .chunks_exact(2)
        .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
        .collect();
    Ok((input, versions))
}

fn parse_key_share(input: &[u8]) -> IResult<&[u8], Vec<KeyShareEntry>> {
    let (input, len) = be_u16(input)?;
    let (input, data) = take(len)(input)?;
    let mut entries = Vec::new();
    let mut remaining = data;

    while !remaining.is_empty() {
        let (rest, (group, key_exchange_len)) = tuple((be_u16, be_u16))(remaining)?;
        let (rest, key_exchange) = take(key_exchange_len)(rest)?;
        entries.push(KeyShareEntry {
            group,
            key_exchange: key_exchange.to_vec(),
        });
        remaining = rest;
    }

    Ok((input, entries))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_client_hello(&data) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Failed to parse ClientHello: {:?}", e),
    }
}