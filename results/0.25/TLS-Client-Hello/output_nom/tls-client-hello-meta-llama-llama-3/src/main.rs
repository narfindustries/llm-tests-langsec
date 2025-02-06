use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
enum CompressionMethod {
    Null,
    Deflate,
    Other(u8),
}

#[derive(Debug, PartialEq)]
enum CipherSuite {
    TlsAes128GcmSha256,
    TlsAes256GcmSha384,
    TlsChacha20Poly1305Sha256,
    TlsAes128CcmSha256,
    TlsAes128Ccm8Sha256,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum ExtensionType {
    SupportedVersions,
    SupportedGroups,
    SignatureAlgorithms,
    KeyShare,
    PskKeyExchangeModes,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum SupportedVersion {
    Tls13,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum NamedGroup {
    Secp256r1,
    Secp384r1,
    Secp521r1,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum SignatureAlgorithm {
    RsaPkcs1Sha256,
    RsaPkcs1Sha384,
    RsaPkcs1Sha512,
    Other(u16),
}

#[derive(Debug, PartialEq)]
struct KeyShareEntry {
    group: NamedGroup,
    key_exchange: Vec<u8>,
}

#[derive(Debug, PartialEq)]
enum PskKeyExchangeMode {
    PskModeDhe,
    Other(u8),
}

#[derive(Debug, PartialEq)]
struct Extension {
    extension_type: ExtensionType,
    extension_data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct ClientHello {
    legacy_version: (u8, u8),
    random: Vec<u8>,
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<CipherSuite>,
    compression_methods: Vec<CompressionMethod>,
    extensions: Vec<Extension>,
}

fn parse_legacy_version(input: &[u8]) -> IResult<&[u8], (u8, u8)> {
    map(take(2usize), |bytes: &[u8]| (bytes[0], bytes[1]))(input)
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take(32usize), |bytes: &[u8]| bytes.to_vec())(input)
}

fn parse_legacy_session_id(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map_res(take_while_m_n(1, 32, |byte| byte != 0), |bytes: &[u8]| {
        Ok(bytes.to_vec())
    })(input)
}

fn parse_cipher_suite(input: &[u8]) -> IResult<&[u8], CipherSuite> {
    map(be_u16, |value: u16| match value {
        0x1301 => CipherSuite::TlsAes128GcmSha256,
        0x1302 => CipherSuite::TlsAes256GcmSha384,
        0x1303 => CipherSuite::TlsChacha20Poly1305Sha256,
        0x1304 => CipherSuite::TlsAes128CcmSha256,
        0x1305 => CipherSuite::TlsAes128Ccm8Sha256,
        _ => CipherSuite::Other(value),
    })(input)
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<CipherSuite>> {
    let (input, length) = be_u16(input)?;
    let (input, cipher_suites) = take(length / 2)(input)?;
    let mut result = Vec::new();
    let mut remaining = cipher_suites;
    while !remaining.is_empty() {
        let (new_remaining, cipher_suite) = parse_cipher_suite(remaining)?;
        result.push(cipher_suite);
        remaining = new_remaining;
    }
    Ok((input, result))
}

fn parse_compression_method(input: &[u8]) -> IResult<&[u8], CompressionMethod> {
    map(be_u8, |value: u8| match value {
        0x00 => CompressionMethod::Null,
        0x01 => CompressionMethod::Deflate,
        _ => CompressionMethod::Other(value),
    })(input)
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<CompressionMethod>> {
    let (input, length) = be_u8(input)?;
    let (input, compression_methods) = take(length)(input)?;
    let mut result = Vec::new();
    let mut remaining = compression_methods;
    while !remaining.is_empty() {
        let (new_remaining, compression_method) = parse_compression_method(remaining)?;
        result.push(compression_method);
        remaining = new_remaining;
    }
    Ok((input, result))
}

fn parse_extension_type(input: &[u8]) -> IResult<&[u8], ExtensionType> {
    map(be_u16, |value: u16| match value {
        43 => ExtensionType::SupportedVersions,
        10 => ExtensionType::SupportedGroups,
        13 => ExtensionType::SignatureAlgorithms,
        51 => ExtensionType::KeyShare,
        45 => ExtensionType::PskKeyExchangeModes,
        _ => ExtensionType::Other(value),
    })(input)
}

fn parse_supported_version(input: &[u8]) -> IResult<&[u8], SupportedVersion> {
    map(be_u16, |value: u16| match value {
        0x0304 => SupportedVersion::Tls13,
        _ => SupportedVersion::Other(value),
    })(input)
}

fn parse_supported_versions(input: &[u8]) -> IResult<&[u8], Vec<SupportedVersion>> {
    let (input, length) = be_u16(input)?;
    let (input, supported_versions) = take(length / 2)(input)?;
    let mut result = Vec::new();
    let mut remaining = supported_versions;
    while !remaining.is_empty() {
        let (new_remaining, supported_version) = parse_supported_version(remaining)?;
        result.push(supported_version);
        remaining = new_remaining;
    }
    Ok((input, result))
}

fn parse_named_group(input: &[u8]) -> IResult<&[u8], NamedGroup> {
    map(be_u16, |value: u16| match value {
        0x0017 => NamedGroup::Secp256r1,
        0x0018 => NamedGroup::Secp384r1,
        0x0019 => NamedGroup::Secp521r1,
        _ => NamedGroup::Other(value),
    })(input)
}

fn parse_supported_groups(input: &[u8]) -> IResult<&[u8], Vec<NamedGroup>> {
    let (input, length) = be_u16(input)?;
    let (input, supported_groups) = take(length / 2)(input)?;
    let mut result = Vec::new();
    let mut remaining = supported_groups;
    while !remaining.is_empty() {
        let (new_remaining, supported_group) = parse_named_group(remaining)?;
        result.push(supported_group);
        remaining = new_remaining;
    }
    Ok((input, result))
}

fn parse_signature_algorithm(input: &[u8]) -> IResult<&[u8], SignatureAlgorithm> {
    map(be_u16, |value: u16| match value {
        0x0401 => SignatureAlgorithm::RsaPkcs1Sha256,
        0x0402 => SignatureAlgorithm::RsaPkcs1Sha384,
        0x0403 => SignatureAlgorithm::RsaPkcs1Sha512,
        _ => SignatureAlgorithm::Other(value),
    })(input)
}

fn parse_signature_algorithms(input: &[u8]) -> IResult<&[u8], Vec<SignatureAlgorithm>> {
    let (input, length) = be_u16(input)?;
    let (input, signature_algorithms) = take(length / 2)(input)?;
    let mut result = Vec::new();
    let mut remaining = signature_algorithms;
    while !remaining.is_empty() {
        let (new_remaining, signature_algorithm) = parse_signature_algorithm(remaining)?;
        result.push(signature_algorithm);
        remaining = new_remaining;
    }
    Ok((input, result))
}

fn parse_key_share_entry(input: &[u8]) -> IResult<&[u8], KeyShareEntry> {
    let (input, group) = parse_named_group(input)?;
    let (input, length) = be_u16(input)?;
    let (input, key_exchange) = take(length)(input)?;
    Ok((input, KeyShareEntry { group, key_exchange: key_exchange.to_vec() }))
}

fn parse_key_share(input: &[u8]) -> IResult<&[u8], Vec<KeyShareEntry>> {
    let (input, length) = be_u16(input)?;
    let (input, key_share) = take(length)(input)?;
    let mut result = Vec::new();
    let mut remaining = key_share;
    while !remaining.is_empty() {
        let (new_remaining, key_share_entry) = parse_key_share_entry(remaining)?;
        result.push(key_share_entry);
        remaining = new_remaining;
    }
    Ok((input, result))
}

fn parse_psk_key_exchange_mode(input: &[u8]) -> IResult<&[u8], PskKeyExchangeMode> {
    map(be_u8, |value: u8| match value {
        0x01 => PskKeyExchangeMode::PskModeDhe,
        _ => PskKeyExchangeMode::Other(value),
    })(input)
}

fn parse_psk_key_exchange_modes(input: &[u8]) -> IResult<&[u8], Vec<PskKeyExchangeMode>> {
    let (input, length) = be_u8(input)?;
    let (input, psk_key_exchange_modes) = take(length)(input)?;
    let mut result = Vec::new();
    let mut remaining = psk_key_exchange_modes;
    while !remaining.is_empty() {
        let (new_remaining, psk_key_exchange_mode) = parse_psk_key_exchange_mode(remaining)?;
        result.push(psk_key_exchange_mode);
        remaining = new_remaining;
    }
    Ok((input, result))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = parse_extension_type(input)?;
    let (input, length) = be_u16(input)?;
    let (input, extension_data) = take(length)(input)?;
    Ok((input, Extension { extension_type, extension_data: extension_data.to_vec() }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (input, length) = be_u16(input)?;
    let (input, extensions) = take(length)(input)?;
    let mut result = Vec::new();
    let mut remaining = extensions;
    while !remaining.is_empty() {
        let (new_remaining, extension) = parse_extension(remaining)?;
        result.push(extension);
        remaining = new_remaining;
    }
    Ok((input, result))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = parse_legacy_version(input)?;
    let (input, random) = parse_random(input)?;
    let (input, legacy_session_id) = parse_legacy_session_id(input)?;
    let (input, cipher_suites) = parse_cipher_suites(input)?;
    let (input, compression_methods) = parse_compression_methods(input)?;
    let (input, extensions) = parse_extensions(input)?;
    Ok((input, ClientHello {
        legacy_version,
        random,
        legacy_session_id,
        cipher_suites,
        compression_methods,
        extensions,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let file = File::open(input_file).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (_remaining, client_hello) = parse_client_hello(&input).unwrap();
    println!("{:?}", client_hello);
}