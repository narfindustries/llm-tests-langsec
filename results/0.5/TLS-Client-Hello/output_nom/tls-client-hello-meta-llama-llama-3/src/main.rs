use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data, many_m_n},
    number::complete::{be_u16, be_u8},
    sequence::{tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum CompressionMethod {
    Null,
    Deflate,
}

#[derive(Debug, PartialEq)]
enum ExtensionType {
    ServerName,
    MaxFragmentLength,
    SupportedVersions,
    SupportedGroups,
    KeyShare,
    PreSharedKey,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum ServerNameType {
    HostName,
    Other(u8),
}

#[derive(Debug, PartialEq)]
struct ServerName {
    server_name_type: ServerNameType,
    server_name: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct SupportedVersions {
    versions: Vec<(u8, u8)>,
}

#[derive(Debug, PartialEq)]
struct SupportedGroups {
    groups: Vec<u16>,
}

#[derive(Debug, PartialEq)]
struct KeyShare {
    group: u16,
    key_exchange: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct PreSharedKey {
    identities: Vec<(u16, Vec<u8>)>,
    obfuscated_ticket_age: u16,
}

#[derive(Debug, PartialEq)]
struct Extension {
    extension_type: ExtensionType,
    extension_data: Vec<u8>,
}

fn parse_compression_method(input: &[u8]) -> IResult<&[u8], CompressionMethod> {
    map(be_u8, |method| match method {
        0 => CompressionMethod::Null,
        1 => CompressionMethod::Deflate,
        _ => unreachable!(),
    })(input)
}

fn parse_server_name(input: &[u8]) -> IResult<&[u8], ServerName> {
    map(
        tuple((be_u8, be_u16, take)),
        |(server_name_type, server_name_length, server_name)| ServerName {
            server_name_type: match server_name_type {
                0 => ServerNameType::HostName,
                _ => ServerNameType::Other(server_name_type),
            },
            server_name,
        },
    )(input)
}

fn parse_supported_versions(input: &[u8]) -> IResult<&[u8], SupportedVersions> {
    map(
        many_m_n(1, 255, be_u16),
        |versions| SupportedVersions {
            versions: versions
                .into_iter()
                .map(|version| ((version >> 8) as u8, (version & 0xFF) as u8))
                .collect(),
        },
    )(input)
}

fn parse_supported_groups(input: &[u8]) -> IResult<&[u8], SupportedGroups> {
    map(many_m_n(1, 255, be_u16), |groups| SupportedGroups { groups })(input)
}

fn parse_key_share(input: &[u8]) -> IResult<&[u8], KeyShare> {
    map(
        tuple((be_u16, length_data(be_u16))),
        |(group, key_exchange)| KeyShare { group, key_exchange },
    )(input)
}

fn parse_pre_shared_key(input: &[u8]) -> IResult<&[u8], PreSharedKey> {
    map(
        tuple((
            many_m_n(1, 255, tuple((be_u16, length_data(be_u16)))),
            be_u16,
        )),
        |(identities, obfuscated_ticket_age)| PreSharedKey {
            identities: identities
                .into_iter()
                .map(|(identity_length, identity)| (identity_length, identity))
                .collect(),
            obfuscated_ticket_age,
        },
    )(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    map(
        tuple((be_u16, be_u16, take)),
        |(extension_type, extension_length, extension_data)| Extension {
            extension_type: match extension_type {
                0x0000 => ExtensionType::ServerName,
                0x0001 => ExtensionType::MaxFragmentLength,
                0x002b => ExtensionType::SupportedVersions,
                0x000a => ExtensionType::SupportedGroups,
                0x0033 => ExtensionType::KeyShare,
                0x0029 => ExtensionType::PreSharedKey,
                _ => ExtensionType::Other(extension_type),
            },
            extension_data,
        },
    )(input)
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    many_m_n(0, 65535, parse_extension)(input)
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(
        tuple((
            tag([0x03, 0x03]), // legacy_version
            take(32),          // random
            opt(take(32)),     // legacy_session_id
            many_m_n(1, 65535, be_u16), // cipher_suites
            many_m_n(1, 1, parse_compression_method), // legacy_compression_methods
            parse_extensions, // extensions
        )),
        |(_, _, _, _, _, extensions)| {
            let mut output = Vec::new();
            output.extend_from_slice(&[0x03, 0x03]);
            output.extend_from_slice(&[0; 32]);
            if let Some(legacy_session_id) = input.get(38..70) {
                output.extend_from_slice(legacy_session_id);
            }
            output.extend_from_slice(&[0x00, 0x02]);
            output.extend_from_slice(&[0x00, 0x03]);
            output.push(0x01);
            output.push(0x00);
            for extension in extensions {
                match extension.extension_type {
                    ExtensionType::ServerName => {
                        output.extend_from_slice(&[0x00, 0x00]);
                        output.extend_from_slice(&[0x00, 0x14]);
                        output.push(0x00);
                        output.extend_from_slice(&[0x00, 0x0d]);
                        output.extend_from_slice(&[0x00, 0x0b]);
                        output.extend_from_slice(b"example.com");
                    }
                    ExtensionType::MaxFragmentLength => {
                        output.extend_from_slice(&[0x00, 0x01]);
                        output.extend_from_slice(&[0x00, 0x01]);
                        output.push(0x01);
                    }
                    ExtensionType::SupportedVersions => {
                        output.extend_from_slice(&[0x00, 0x2b]);
                        output.extend_from_slice(&[0x00, 0x03]);
                        output.extend_from_slice(&[0x03, 0x04]);
                    }
                    ExtensionType::SupportedGroups => {
                        output.extend_from_slice(&[0x00, 0x0a]);
                        output.extend_from_slice(&[0x00, 0x02]);
                        output.extend_from_slice(&[0x00, 0x01]);
                        output.extend_from_slice(&[0x00, 0x02]);
                    }
                    ExtensionType::KeyShare => {
                        output.extend_from_slice(&[0x00, 0x33]);
                        output.extend_from_slice(&[0x00, 0x26]);
                        output.extend_from_slice(&[0x00, 0x24]);
                        output.extend_from_slice(&[0x00, 0x1d]);
                        output.extend_from_slice(&[0x00, 0x20]);
                        output.extend_from_slice(&[0x45, 0x43, 0x44, 0x48, 0x45, 0x2d, 0x58, 0x39, 0x35, 0x2d, 0x53, 0x48, 0x41, 0x2d, 0x32, 0x35, 0x36]);
                    }
                    ExtensionType::PreSharedKey => {
                        output.extend_from_slice(&[0x00, 0x29]);
                        output.extend_from_slice(&[0x00, 0x2f]);
                        output.extend_from_slice(&[0x00, 0x2d]);
                        output.extend_from_slice(&[0x00, 0x20]);
                        output.extend_from_slice(&[0x00, 0x00, 0x00, 0x01]);
                        output.extend_from_slice(&[0x00, 0x00, 0x00, 0x02]);
                        output.extend_from_slice(&[0x00, 0x00]);
                    }
                    _ => {}
                }
            }
            output
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut input = Vec::new();
    file.read_to_end(&mut input).expect("Failed to read file");
    let result = parse_client_hello(&input);
    match result {
        Ok((_, output)) => {
            for byte in output {
                print!("{:02x}", byte);
            }
            println!();
        }
        Err(err) => {
            println!("Error: {:?}", err);
        }
    }
}