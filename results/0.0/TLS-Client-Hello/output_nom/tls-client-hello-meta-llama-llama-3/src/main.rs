use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u24, be_u8},
    sequence::{tuple, preceded},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, BufReader},
    path::Path,
};

#[derive(Debug)]
enum CipherSuite {
    TLS_NULL_WITH_NULL_NULL,
    TLS_RSA_WITH_NULL_MD5,
    TLS_RSA_WITH_NULL_SHA,
    // Add more cipher suites as needed
    Unknown(u16),
}

#[derive(Debug)]
enum CompressionMethod {
    NULL,
    Unknown(u8),
}

#[derive(Debug)]
enum ExtensionType {
    ServerName,
    MaxFragmentLength,
    ClientCertificateUrl,
    TrustedCaKeys,
    TruncatedHmac,
    StatusRequest,
    UserMapping,
    ClientAuthz,
    ServerAuthz,
    CertType,
    SupportedGroups,
    EcPointFormats,
    Srtp,
    Heartbeat,
    ApplicationLayerProtocolNegotiation,
    StatusRequestV2,
    SignedCertificateTimestamp,
    ClientCertificateType,
    ServerCertificateType,
    Padding,
    Unknown(u16),
}

#[derive(Debug)]
struct Extension {
    extension_type: ExtensionType,
    extension_data: Vec<u8>,
}

#[derive(Debug)]
struct ClientHello {
    record_layer_version: (u8, u8),
    random: [u8; 32],
    session_id: Vec<u8>,
    cipher_suites: Vec<CipherSuite>,
    compression_methods: Vec<CompressionMethod>,
    extensions: Vec<Extension>,
}

fn parse_cipher_suite(input: &[u8]) -> IResult<&[u8], CipherSuite> {
    map(be_u16, |value| match value {
        0x0000 => CipherSuite::TLS_NULL_WITH_NULL_NULL,
        0x0001 => CipherSuite::TLS_RSA_WITH_NULL_MD5,
        0x0002 => CipherSuite::TLS_RSA_WITH_NULL_SHA,
        // Add more cipher suites as needed
        _ => CipherSuite::Unknown(value),
    })(input)
}

fn parse_compression_method(input: &[u8]) -> IResult<&[u8], CompressionMethod> {
    map(be_u8, |value| match value {
        0x00 => CompressionMethod::NULL,
        _ => CompressionMethod::Unknown(value),
    })(input)
}

fn parse_extension_type(input: &[u8]) -> IResult<&[u8], ExtensionType> {
    map(be_u16, |value| match value {
        0x0000 => ExtensionType::ServerName,
        0x0001 => ExtensionType::MaxFragmentLength,
        0x0002 => ExtensionType::ClientCertificateUrl,
        0x0003 => ExtensionType::TrustedCaKeys,
        0x0004 => ExtensionType::TruncatedHmac,
        0x0005 => ExtensionType::StatusRequest,
        0x0006 => ExtensionType::UserMapping,
        0x0007 => ExtensionType::ClientAuthz,
        0x0008 => ExtensionType::ServerAuthz,
        0x0009 => ExtensionType::CertType,
        0x000a => ExtensionType::SupportedGroups,
        0x000b => ExtensionType::EcPointFormats,
        0x000c => ExtensionType::Srtp,
        0x000d => ExtensionType::Heartbeat,
        0x0010 => ExtensionType::ApplicationLayerProtocolNegotiation,
        0x0011 => ExtensionType::StatusRequestV2,
        0x0012 => ExtensionType::SignedCertificateTimestamp,
        0x0013 => ExtensionType::ClientCertificateType,
        0x0014 => ExtensionType::ServerCertificateType,
        0x0015 => ExtensionType::Padding,
        _ => ExtensionType::Unknown(value),
    })(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, (extension_type, extension_data_length)) = tuple((parse_extension_type, be_u16))(input)?;
    let (input, extension_data) = length_data(be_u16)(input)?;
    Ok((input, Extension {
        extension_type,
        extension_data,
    }))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, record_layer_version) = tuple((be_u8, be_u8))(input)?;
    let (input, random) = take(32u8)(input)?;
    let (input, session_id_length) = be_u8(input)?;
    let (input, session_id) = take(session_id_length as usize)(input)?;
    let (input, cipher_suites_length) = be_u16(input)?;
    let (input, cipher_suites) = many0(parse_cipher_suite)(input)?;
    let (input, compression_methods_length) = be_u8(input)?;
    let (input, compression_methods) = many0(parse_compression_method)(input)?;
    let (input, extensions_length) = opt(be_u16)(input)?;
    let (input, extensions) = match extensions_length {
        Some(length) => many0(parse_extension)(input)?,
        None => vec![],
    };
    Ok((input, ClientHello {
        record_layer_version,
        random: random.try_into().unwrap(),
        session_id: session_id.to_vec(),
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
    let input_file = &args[1];
    let path = Path::new(input_file);
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (_remaining, client_hello) = parse_client_hello(&input).unwrap();
    println!("{:?}", client_hello);
}