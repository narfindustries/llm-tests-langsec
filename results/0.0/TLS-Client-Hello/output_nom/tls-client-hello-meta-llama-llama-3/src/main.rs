use nom::{
    bytes::complete::{take},
    combinator::{map},
    multi::{length_data, many0},
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
}

#[derive(Debug, PartialEq)]
enum CipherSuite {
    TlsAes128GcmSha256,
    TlsAes256GcmSha384,
    TlsChacha20Poly1305Sha256,
    TlsAes128CcmSha256,
    TlsAes128Ccm8Sha256,
}

#[derive(Debug, PartialEq)]
enum ExtensionType {
    SupportedVersions,
    SupportedGroups,
    KeyShare,
    PreSharedKey,
    EarlyData,
    Cookie,
    PSKKeyExchangeModes,
    TicketEarlyDataInfo,
    SignatureAlgorithms,
    SignatureAlgorithmsCert,
}

#[derive(Debug, PartialEq)]
enum SupportedVersion {
    Tls13,
}

#[derive(Debug, PartialEq)]
enum SupportedGroup {
    Secp256r1,
    Secp384r1,
    Secp521r1,
    X25519,
    X448,
}

#[derive(Debug, PartialEq)]
enum KeyShareEntry {
    Secp256r1,
    Secp384r1,
    Secp521r1,
    X25519,
    X448,
}

#[derive(Debug, PartialEq)]
enum PreSharedKeyIdentity {
    Identity,
}

#[derive(Debug, PartialEq)]
enum PSKKeyExchangeMode {
    PSKKE,
    PSKDHEKE,
}

#[derive(Debug, PartialEq)]
enum SignatureAlgorithm {
    RsaPkcs1Sha256,
    RsaPkcs1Sha384,
    RsaPkcs1Sha512,
    RsaPssSha256,
    RsaPssSha384,
    RsaPssSha512,
    EcdsaSecp256r1Sha256,
    EcdsaSecp384r1Sha384,
    EcdsaSecp521r1Sha512,
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

#[derive(Debug, PartialEq)]
struct Extension {
    extension_type: ExtensionType,
    extension_data: Vec<u8>,
}

fn parse_legacy_version(input: &[u8]) -> IResult<&[u8], (u8, u8)> {
    tuple((be_u8, be_u8))(input)
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take(32u8), |x: &[u8]| x.to_vec())(input)
}

fn parse_legacy_session_id(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(length_data(be_u8), |x: &[u8]| x.to_vec())(input)
}

fn parse_cipher_suite(input: &[u8]) -> IResult<&[u8], CipherSuite> {
    map(be_u16, |suite: u16| match suite {
        0x1301 => CipherSuite::TlsAes128GcmSha256,
        0x1302 => CipherSuite::TlsAes256GcmSha384,
        0x1303 => CipherSuite::TlsChacha20Poly1305Sha256,
        0x1304 => CipherSuite::TlsAes128CcmSha256,
        0x1305 => CipherSuite::TlsAes128Ccm8Sha256,
        _ => panic!("Unknown cipher suite"),
    })(input)
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<CipherSuite>> {
    many0(parse_cipher_suite)(input)
}

fn parse_compression_method(input: &[u8]) -> IResult<&[u8], CompressionMethod> {
    map(be_u8, |method: u8| match method {
        0x00 => CompressionMethod::Null,
        _ => panic!("Unknown compression method"),
    })(input)
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<CompressionMethod>> {
    many0(parse_compression_method)(input)
}

fn parse_extension_type(input: &[u8]) -> IResult<&[u8], ExtensionType> {
    map(be_u16, |extension_type: u16| match extension_type {
        0x002b => ExtensionType::SupportedVersions,
        0x000a => ExtensionType::SupportedGroups,
        0x0033 => ExtensionType::KeyShare,
        0x0029 => ExtensionType::PreSharedKey,
        0x002a => ExtensionType::EarlyData,
        0x0026 => ExtensionType::Cookie,
        0x002d => ExtensionType::PSKKeyExchangeModes,
        0x002e => ExtensionType::TicketEarlyDataInfo,
        0x000d => ExtensionType::SignatureAlgorithms,
        0x000e => ExtensionType::SignatureAlgorithmsCert,
        _ => panic!("Unknown extension type"),
    })(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = parse_extension_type(input)?;
    let (input, extension_data) = length_data(be_u16)(input)?;
    Ok((input, Extension {
        extension_type,
        extension_data: extension_data.to_vec(),
    }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    many0(parse_extension)(input)
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
        panic!("Usage: {} <input_file>", args[0]);
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut input = Vec::new();
    file.read_to_end(&mut input).expect("Failed to read file");
    match parse_client_hello(&input) {
        Ok((_, client_hello)) => println!("{:?}", client_hello),
        Err(err) => panic!("Failed to parse client hello: {:?}", err),
    }
}