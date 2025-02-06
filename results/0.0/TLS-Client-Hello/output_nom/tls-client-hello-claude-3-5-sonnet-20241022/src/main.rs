use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u8},
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
    PreSharedKey(PreSharedKey),
    EarlyData,
    Cookie(Vec<u8>),
    Padding(Vec<u8>),
    Unknown(u16, Vec<u8>),
}

#[derive(Debug)]
struct KeyShareEntry {
    group: u16,
    key_exchange: Vec<u8>,
}

#[derive(Debug)]
struct PreSharedKey {
    identities: Vec<PskIdentity>,
    binders: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct PskIdentity {
    identity: Vec<u8>,
    obfuscated_ticket_age: u16,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = verify(be_u16, |&x| x == 0x0303)(input)?;
    let (input, random) = take(32u8)(input)?;
    let (input, legacy_session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites_bytes) = length_data(be_u16)(input)?;
    let (_, cipher_suites) = many0(be_u16)(cipher_suites_bytes)?;
    let (input, compression_methods) = length_data(be_u8)(input)?;
    let (input, extensions_bytes) = length_data(be_u16)(input)?;
    let (_, extensions) = many0(parse_extension)(extensions_bytes)?;

    Ok((
        input,
        ClientHello {
            legacy_version,
            random: random.to_vec(),
            legacy_session_id: legacy_session_id.to_vec(),
            cipher_suites,
            compression_methods: compression_methods.to_vec(),
            extensions,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_data) = length_data(be_u16)(input)?;

    match extension_type {
        0x0000 => map(parse_server_name, Extension::ServerName)(extension_data),
        0x000a => map(parse_supported_groups, Extension::SupportedGroups)(extension_data),
        0x000d => map(parse_signature_algorithms, Extension::SignatureAlgorithms)(extension_data),
        0x0033 => map(parse_key_share, Extension::KeyShare)(extension_data),
        0x002b => map(parse_supported_versions, Extension::SupportedVersions)(extension_data),
        0x002d => map(parse_psk_key_exchange_modes, Extension::PskKeyExchangeModes)(extension_data),
        0x0029 => map(parse_pre_shared_key, Extension::PreSharedKey)(extension_data),
        0x002a => Ok((extension_data, Extension::EarlyData)),
        0x002c => Ok((extension_data, Extension::Cookie(extension_data.to_vec()))),
        0x0015 => Ok((extension_data, Extension::Padding(extension_data.to_vec()))),
        _ => Ok((extension_data, Extension::Unknown(extension_type, extension_data.to_vec()))),
    }
}

fn parse_server_name(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = be_u16(input)?;
    let (input, _) = tag(&[0x00])(input)?;
    let (input, hostname) = length_data(be_u16)(input)?;
    Ok((
        input,
        String::from_utf8_lossy(hostname).to_string(),
    ))
}

fn parse_supported_groups(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (_, groups_bytes) = length_data(be_u16)(input)?;
    many0(be_u16)(groups_bytes)
}

fn parse_signature_algorithms(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (_, algs_bytes) = length_data(be_u16)(input)?;
    many0(be_u16)(algs_bytes)
}

fn parse_key_share(input: &[u8]) -> IResult<&[u8], Vec<KeyShareEntry>> {
    let (_, entries_bytes) = length_data(be_u16)(input)?;
    many0(parse_key_share_entry)(entries_bytes)
}

fn parse_key_share_entry(input: &[u8]) -> IResult<&[u8], KeyShareEntry> {
    let (input, group) = be_u16(input)?;
    let (input, key_exchange) = length_data(be_u16)(input)?;
    Ok((
        input,
        KeyShareEntry {
            group,
            key_exchange: key_exchange.to_vec(),
        },
    ))
}

fn parse_supported_versions(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (_, versions_bytes) = length_data(be_u8)(input)?;
    many0(be_u16)(versions_bytes)
}

fn parse_psk_key_exchange_modes(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, modes_bytes) = length_data(be_u8)(input)?;
    Ok((input, modes_bytes.to_vec()))
}

fn parse_pre_shared_key(input: &[u8]) -> IResult<&[u8], PreSharedKey> {
    let (input, identities_bytes) = length_data(be_u16)(input)?;
    let (input, binders_bytes) = length_data(be_u16)(input)?;
    
    let (_, identities) = many0(parse_psk_identity)(identities_bytes)?;
    let (_, binders) = many0(length_data(be_u8))(binders_bytes)?;
    
    Ok((
        input,
        PreSharedKey {
            identities,
            binders: binders.iter().map(|b| b.to_vec()).collect(),
        },
    ))
}

fn parse_psk_identity(input: &[u8]) -> IResult<&[u8], PskIdentity> {
    let (input, identity) = length_data(be_u16)(input)?;
    let (input, obfuscated_ticket_age) = be_u16(input)?;
    Ok((
        input,
        PskIdentity {
            identity: identity.to_vec(),
            obfuscated_ticket_age,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_client_hello(&buffer) {
        Ok((remaining, client_hello)) => {
            println!("Parsed ClientHello: {:#?}", client_hello);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => eprintln!("Failed to parse ClientHello: {:?}", e),
    }
}