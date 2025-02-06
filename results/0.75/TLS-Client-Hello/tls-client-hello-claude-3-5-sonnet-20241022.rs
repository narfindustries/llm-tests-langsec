use nom::bytes::complete::{take};
use nom::number::complete::{be_u16, be_u8};
use nom::IResult;
use std::env;
use std::fs;

#[derive(Debug)]
struct ClientHello<'a> {
    legacy_version: u16,
    random: &'a [u8],
    legacy_session_id: &'a [u8],
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension<'a>>,
}

#[derive(Debug)]
struct Extension<'a> {
    extension_type: u16,
    extension_data: ExtensionData<'a>,
}

#[derive(Debug)]
enum ExtensionData<'a> {
    SupportedVersions(Vec<u16>),
    SupportedGroups(Vec<u16>),
    SignatureAlgorithms(Vec<u16>),
    KeyShare(Vec<KeyShareEntry<'a>>),
    ServerName(Vec<ServerName<'a>>),
    PskKeyExchangeModes(Vec<u8>),
    PreSharedKey(PreSharedKeyExtension<'a>),
    EarlyData(Option<u32>),
    Unknown(&'a [u8]),
}

#[derive(Debug)]
struct KeyShareEntry<'a> {
    group: u16,
    key_exchange: &'a [u8],
}

#[derive(Debug)]
struct ServerName<'a> {
    name_type: u8,
    name: &'a [u8],
}

#[derive(Debug)]
struct PreSharedKeyExtension<'a> {
    identities: Vec<PskIdentity<'a>>,
    binders: Vec<&'a [u8]>,
}

#[derive(Debug)]
struct PskIdentity<'a> {
    identity: &'a [u8],
    obfuscated_ticket_age: u32,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32u8)(input)?;
    let (input, session_id_len) = be_u8(input)?;
    let (input, legacy_session_id) = take(session_id_len)(input)?;
    
    let (input, cipher_suites_len) = be_u16(input)?;
    let (mut input_cs, cipher_suites_data) = take(cipher_suites_len)(input)?;
    let mut cipher_suites = Vec::new();
    let mut cs_data = cipher_suites_data;
    while !cs_data.is_empty() {
        let (remaining, suite) = be_u16(cs_data)?;
        cipher_suites.push(suite);
        cs_data = remaining;
    }

    let (input, compression_methods_len) = be_u8(input_cs)?;
    let (input, compression_methods_data) = take(compression_methods_len)(input)?;
    let compression_methods = compression_methods_data.to_vec();

    let (input, extensions_len) = be_u16(input)?;
    let (input, extensions_data) = take(extensions_len)(input)?;
    let (_, extensions) = parse_extensions(extensions_data)?;

    Ok((input, ClientHello {
        legacy_version,
        random,
        legacy_session_id,
        cipher_suites,
        compression_methods,
        extensions,
    }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let mut extensions = Vec::new();
    let mut remaining = input;

    while !remaining.is_empty() {
        let (input, extension_type) = be_u16(remaining)?;
        let (input, extension_len) = be_u16(input)?;
        let (input, extension_data) = take(extension_len)(input)?;

        let extension_data = match extension_type {
            0x002b => {
                let (_, versions) = parse_supported_versions(extension_data)?;
                ExtensionData::SupportedVersions(versions)
            }
            0x000a => {
                let (_, groups) = parse_supported_groups(extension_data)?;
                ExtensionData::SupportedGroups(groups)
            }
            0x000d => {
                let (_, algorithms) = parse_signature_algorithms(extension_data)?;
                ExtensionData::SignatureAlgorithms(algorithms)
            }
            0x0033 => {
                let (_, entries) = parse_key_share(extension_data)?;
                ExtensionData::KeyShare(entries)
            }
            0x0000 => {
                let (_, server_names) = parse_server_name(extension_data)?;
                ExtensionData::ServerName(server_names)
            }
            0x002d => {
                let (_, modes) = parse_psk_key_exchange_modes(extension_data)?;
                ExtensionData::PskKeyExchangeModes(modes)
            }
            0x0029 => {
                let (_, psk) = parse_pre_shared_key(extension_data)?;
                ExtensionData::PreSharedKey(psk)
            }
            0x002a => {
                let (_, early_data) = parse_early_data(extension_data)?;
                ExtensionData::EarlyData(early_data)
            }
            _ => ExtensionData::Unknown(extension_data),
        };

        extensions.push(Extension {
            extension_type,
            extension_data,
        });

        remaining = input;
    }

    Ok((remaining, extensions))
}

fn parse_supported_versions(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, len) = be_u8(input)?;
    let mut versions = Vec::new();
    let mut remaining = input;
    
    for _ in 0..(len / 2) {
        let (input, version) = be_u16(remaining)?;
        versions.push(version);
        remaining = input;
    }
    
    Ok((remaining, versions))
}

fn parse_supported_groups(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, len) = be_u16(input)?;
    let mut groups = Vec::new();
    let mut remaining = input;
    
    for _ in 0..(len / 2) {
        let (input, group) = be_u16(remaining)?;
        groups.push(group);
        remaining = input;
    }
    
    Ok((remaining, groups))
}

fn parse_signature_algorithms(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, len) = be_u16(input)?;
    let mut algorithms = Vec::new();
    let mut remaining = input;
    
    for _ in 0..(len / 2) {
        let (input, algorithm) = be_u16(remaining)?;
        algorithms.push(algorithm);
        remaining = input;
    }
    
    Ok((remaining, algorithms))
}

fn parse_key_share(input: &[u8]) -> IResult<&[u8], Vec<KeyShareEntry>> {
    let (input, len) = be_u16(input)?;
    let (_, entries_data) = take(len)(input)?;
    let mut entries = Vec::new();
    let mut remaining = entries_data;
    
    while !remaining.is_empty() {
        let (input, group) = be_u16(remaining)?;
        let (input, key_exchange_len) = be_u16(input)?;
        let (input, key_exchange) = take(key_exchange_len)(input)?;
        
        entries.push(KeyShareEntry {
            group,
            key_exchange,
        });
        
        remaining = input;
    }
    
    Ok((input, entries))
}

fn parse_server_name(input: &[u8]) -> IResult<&[u8], Vec<ServerName>> {
    let (input, len) = be_u16(input)?;
    let (_, list_data) = take(len)(input)?;
    let mut names = Vec::new();
    let mut remaining = list_data;
    
    while !remaining.is_empty() {
        let (input, name_type) = be_u8(remaining)?;
        let (input, name_len) = be_u16(input)?;
        let (input, name) = take(name_len)(input)?;
        
        names.push(ServerName {
            name_type,
            name,
        });
        
        remaining = input;
    }
    
    Ok((input, names))
}

fn parse_psk_key_exchange_modes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, len) = be_u8(input)?;
    let (input, modes) = take(len)(input)?;
    Ok((input, modes.to_vec()))
}

fn parse_pre_shared_key(input: &[u8]) -> IResult<&[u8], PreSharedKeyExtension> {
    let (input, identities_len) = be_u16(input)?;
    let (input, identities_data) = take(identities_len)(input)?;
    let mut identities = Vec::new();
    let mut remaining = identities_data;
    
    while !remaining.is_empty() {
        let (input, identity_len) = be_u16(remaining)?;
        let (input, identity) = take(identity_len)(input)?;
        let (input, obfuscated_ticket_age) = be_u16(input).map(|(i, v)| (i, v as u32))?;
        
        identities.push(PskIdentity {
            identity,
            obfuscated_ticket_age,
        });
        
        remaining = input;
    }
    
    let (input, binders_len) = be_u16(input)?;
    let (input, binders_data) = take(binders_len)(input)?;
    let mut binders = Vec::new();
    let mut remaining = binders_data;
    
    while !remaining.is_empty() {
        let (input, binder_len) = be_u8(remaining)?;
        let (input, binder) = take(binder_len)(input)?;
        binders.push(binder);
        remaining = input;
    }
    
    Ok((input, PreSharedKeyExtension {
        identities,
        binders,
    }))
}

fn parse_early_data(input: &[u8]) -> IResult<&[u8], Option<u32>> {
    if input.is_empty() {
        Ok((input, None))
    } else {
        let (input, max_early_data_size) = be_u16(input).map(|(i, v)| (i, v as u32))?;
        Ok((input, Some(max_early_data_size)))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let input = fs::read(&args[1]).expect("Failed to read input file");
    match parse_client_hello(&input) {
        Ok((remaining, client_hello)) => {
            println!("Parsed ClientHello: {:#?}", client_hello);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse ClientHello: {:?}", e);
            std::process::exit(1);
        }
    }
}