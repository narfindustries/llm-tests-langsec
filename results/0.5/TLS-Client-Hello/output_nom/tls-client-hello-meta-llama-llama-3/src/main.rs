use nom::{
    bits::complete::{tag, take_bits},
    bytes::complete::{take, take_tuple},
    combinator::{map, opt},
    multi::length_data,
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Seek, SeekFrom},
    path::Path,
};

#[derive(Debug)]
enum CompressionMethod {
    Null,
    Other(u8),
}

#[derive(Debug)]
enum CipherSuite {
    TlsNullWithNullNull,
    TlsRsaWithAes128CbcSha,
    TlsDheRsaWithAes128CbcSha,
    Other(u16),
}

#[derive(Debug)]
enum ExtensionType {
    ServerName,
    StatusRequest,
    SupportedGroups,
    EcPointFormats,
    SessionTicket,
    NextProtocolNegotiation,
    ApplicationLayerProtocolNegotiation,
    SignedCertificateTimestamp,
    ClientCertificateUrl,
    TrustedCaKeys,
    TruncatedHmac,
    CertificateStatusRequest,
    SupplementalData,
    KeyShare,
    PreSharedKey,
    EarlyData,
    SupportedVersions,
    Cookie,
    PskKeyExchangeModes,
    TicketEarlyDataInfo,
    Other(u16),
}

#[derive(Debug)]
enum KeyExchange {
    Rsa,
    DheRsa,
    EcdheRsa,
    RsaPsk,
    EcdhePsk,
    DhePsk,
    PsK,
}

#[derive(Debug)]
enum SignatureScheme {
    RsaPkcs1V15,
    EcdsaSecp256r1Sha256,
    RsaPssRsaesha256,
    Other(u16),
}

fn parse_record_layer(input: &[u8]) -> IResult<&[u8], (&[u8], u16, u16)> {
    tuple((
        take(1u8),
        take(2u8),
        take(2u8),
        length_data(take(2u8)),
    ))(input)
}

fn parse_handshake_header(input: &[u8]) -> IResult<&[u8], (u8, u24, u16, u16)> {
    tuple((
        take(1u8),
        take(3u8),
        take(2u8),
        take(2u8),
    ))(input)
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], (
    u16,
    Vec<u8>,
    Vec<CipherSuite>,
    Vec<CompressionMethod>,
    Option<Vec<u8>>,
    Vec<Extension>,
)> {
    tuple((
        take(2u8),
        length_data(take(1u8)),
        length_data(take(2u8)),
        length_data(take(1u8)),
        opt(preceded(tag([0; 1]), length_data(take(1u8)))),
        length_data(take(2u8)),
    ))(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], (ExtensionType, Vec<u8>)> {
    tuple((
        take(2u8),
        length_data(take(2u8)),
    ))(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut input = Vec::new();
    file.read_to_end(&mut input)?;

    let (_rest, (.record_type, _record_version, record_length, record_data)) =
        parse_record_layer(&input).unwrap();
    let (_rest, (handshake_type, _handshake_length, handshake_version, handshake_random)) =
        parse_handshake_header(record_data).unwrap();
    let (
        _rest,
        (
            _handshake_version,
            random,
            _session_id,
            cipher_suites,
            compression_methods,
            extensions,
        ),
    ) = parse_client_hello(handshake_random).unwrap();

    let cipher_suites: Vec<CipherSuite> = cipher_suites
        .chunks(2)
        .map(|cs| match u16::from_be_bytes([cs[0], cs[1]]) {
            0x0000 => CipherSuite::TlsNullWithNullNull,
            0x002f => CipherSuite::TlsRsaWithAes128CbcSha,
            0x0031 => CipherSuite::TlsDheRsaWithAes128CbcSha,
            x => CipherSuite::Other(x),
        })
        .collect();

    let extensions: Vec<Extension> = extensions
        .chunks(6)
        .map(|ext| {
            let (ext_type, ext_data) = parse_extension(ext).unwrap();
            let ext_type = match u16::from_be_bytes([ext_type.0[0], ext_type.0[1]]) {
                0x0000 => ExtensionType::ServerName,
                0x0001 => ExtensionType::StatusRequest,
                0x000a => ExtensionType::SupportedGroups,
                0x000b => ExtensionType::EcPointFormats,
                0x0023 => ExtensionType::SessionTicket,
                0x3374 => ExtensionType::NextProtocolNegotiation,
                0x10 => ExtensionType::ApplicationLayerProtocolNegotiation,
                0x12 => ExtensionType::SignedCertificateTimestamp,
                0x13 => ExtensionType::ClientCertificateUrl,
                0x14 => ExtensionType::TrustedCaKeys,
                0x15 => ExtensionType::TruncatedHmac,
                0x16 => ExtensionType::CertificateStatusRequest,
                0x17 => ExtensionType::SupplementalData,
                0x33 => ExtensionType::KeyShare,
                0x29 => ExtensionType::PreSharedKey,
                0x2a => ExtensionType::EarlyData,
                0x2b => ExtensionType::SupportedVersions,
                0x2c => ExtensionType::Cookie,
                0x2d => ExtensionType::PskKeyExchangeModes,
                0x2e => ExtensionType::TicketEarlyDataInfo,
                x => ExtensionType::Other(x),
            };
            Extension { ext_type, ext_data }
        })
        .collect();

    println!("{:?}", record_type);
    println!("{:?}", random);
    println!("{:?}", cipher_suites);
    println!("{:?}", compression_methods);
    println!("{:?}", extensions);

    Ok(())
}

#[derive(Debug)]
struct Extension {
    ext_type: ExtensionType,
    ext_data: Vec<u8>,
}