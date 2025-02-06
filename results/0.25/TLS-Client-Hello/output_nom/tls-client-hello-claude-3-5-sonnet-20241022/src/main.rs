use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    multi::length_data,
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
enum Extension {
    ServerName(String),
    SupportedGroups(Vec<u16>),
    SignatureAlgorithms(Vec<u16>),
    KeyShare(Vec<KeyShareEntry>),
    SupportedVersions(Vec<u16>),
    PskKeyExchangeModes(Vec<u8>),
    PreSharedKey(PreSharedKeyExtension),
    EarlyData,
    Cookie(Vec<u8>),
    CertificateAuthorities(Vec<Vec<u8>>),
    PostHandshakeAuth,
    Unknown(u16, Vec<u8>),
}

#[derive(Debug)]
struct KeyShareEntry {
    group: u16,
    key_exchange: Vec<u8>,
}

#[derive(Debug)]
struct PreSharedKeyExtension {
    identities: Vec<PskIdentity>,
    binders: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct PskIdentity {
    identity: Vec<u8>,
    obfuscated_ticket_age: u32,
}

fn parse_key_share_entry(input: &[u8]) -> IResult<&[u8], KeyShareEntry> {
    let (input, group) = be_u16(input)?;
    let (input, key_exchange) = length_data(be_u16)(input)?;
    Ok((input, KeyShareEntry {
        group,
        key_exchange: key_exchange.to_vec(),
    }))
}

fn parse_psk_identity(input: &[u8]) -> IResult<&[u8], PskIdentity> {
    let (input, identity) = length_data(be_u16)(input)?;
    let (input, obfuscated_ticket_age) = be_u16(input)?;
    Ok((input, PskIdentity {
        identity: identity.to_vec(),
        obfuscated_ticket_age: obfuscated_ticket_age as u32,
    }))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_data) = length_data(be_u16)(input)?;
    
    match extension_type {
        0x0000 => {
            let (_, hostname) = length_data(be_u16)(extension_data)?;
            Ok((input, Extension::ServerName(String::from_utf8_lossy(hostname).to_string())))
        }
        0x000a => {
            let (remaining, _) = be_u16(extension_data)?;
            let mut groups = Vec::new();
            let mut current = remaining;
            while !current.is_empty() {
                let (remaining, group) = be_u16(current)?;
                groups.push(group);
                current = remaining;
            }
            Ok((input, Extension::SupportedGroups(groups)))
        }
        0x000d => {
            let (remaining, _) = be_u16(extension_data)?;
            let mut algorithms = Vec::new();
            let mut current = remaining;
            while !current.is_empty() {
                let (remaining, alg) = be_u16(current)?;
                algorithms.push(alg);
                current = remaining;
            }
            Ok((input, Extension::SignatureAlgorithms(algorithms)))
        }
        0x0033 => {
            let (remaining, _) = be_u16(extension_data)?;
            let mut entries = Vec::new();
            let mut current = remaining;
            while !current.is_empty() {
                let (remaining, entry) = parse_key_share_entry(current)?;
                entries.push(entry);
                current = remaining;
            }
            Ok((input, Extension::KeyShare(entries)))
        }
        _ => Ok((input, Extension::Unknown(extension_type, extension_data.to_vec()))),
    }
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, legacy_session_id) = length_data(be_u8)(input)?;
    
    let (input, cipher_suites_data) = length_data(be_u16)(input)?;
    let mut cipher_suites = Vec::new();
    let mut current = cipher_suites_data;
    while !current.is_empty() {
        let (remaining, suite) = be_u16(current)?;
        cipher_suites.push(suite);
        current = remaining;
    }
    
    let (input, compression_methods) = length_data(be_u8)(input)?;
    
    let (input, extensions_data) = length_data(be_u16)(input)?;
    let mut extensions = Vec::new();
    let mut current = extensions_data;
    while !current.is_empty() {
        let (remaining, extension) = parse_extension(current)?;
        extensions.push(extension);
        current = remaining;
    }
    
    Ok((input, ClientHello {
        legacy_version,
        random: random.to_vec(),
        legacy_session_id: legacy_session_id.to_vec(),
        cipher_suites,
        compression_methods: compression_methods.to_vec(),
        extensions,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tls_client_hello_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((remaining, client_hello)) => {
            println!("Parsed ClientHello: {:#?}", client_hello);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse ClientHello: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}