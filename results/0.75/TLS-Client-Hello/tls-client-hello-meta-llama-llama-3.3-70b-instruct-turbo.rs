use nom::{
    bytes::complete::{take, take_while_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug, PartialEq)]
enum HandshakeType {
    HelloRequest,
    ClientHello,
    ServerHello,
    HelloVerifyRequest,
    NewSessionTicket,
    EndOfEarlyData,
    HelloRetryRequest,
    EncryptedExtensions,
    Certificate,
    ServerKeyExchange,
    CertificateRequest,
    ServerHelloDone,
    CertificateVerify,
    ClientKeyExchange,
    Finished,
    KeyUpdate,
    MessageHash,
}

impl From<u8> for HandshakeType {
    fn from(value: u8) -> Self {
        match value {
            0 => HandshakeType::HelloRequest,
            1 => HandshakeType::ClientHello,
            2 => HandshakeType::ServerHello,
            3 => HandshakeType::HelloVerifyRequest,
            4 => HandshakeType::NewSessionTicket,
            5 => HandshakeType::EndOfEarlyData,
            6 => HandshakeType::HelloRetryRequest,
            8 => HandshakeType::EncryptedExtensions,
            11 => HandshakeType::Certificate,
            12 => HandshakeType::ServerKeyExchange,
            13 => HandshakeType::CertificateRequest,
            14 => HandshakeType::ServerHelloDone,
            15 => HandshakeType::CertificateVerify,
            16 => HandshakeType::ClientKeyExchange,
            20 => HandshakeType::Finished,
            24 => HandshakeType::KeyUpdate,
            254 => HandshakeType::MessageHash,
            _ => panic!("Invalid HandshakeType"),
        }
    }
}

#[derive(Debug, PartialEq)]
struct ProtocolVersion {
    major: u8,
    minor: u8,
}

impl ProtocolVersion {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, major) = be_u8(input)?;
        let (input, minor) = be_u8(input)?;
        Ok((input, ProtocolVersion { major, minor }))
    }
}

#[derive(Debug, PartialEq)]
struct Random {
    gmt_unix_time: u32,
    random_bytes: Vec<u8>,
}

impl Random {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, gmt_unix_time) = be_u32(input)?;
        let (input, random_bytes) = take(28u8)(input)?;
        Ok((input, Random {
            gmt_unix_time,
            random_bytes: random_bytes.to_vec(),
        }))
    }
}

#[derive(Debug, PartialEq)]
enum CompressionMethod {
    None,
}

impl From<u8> for CompressionMethod {
    fn from(value: u8) -> Self {
        match value {
            0 => CompressionMethod::None,
            _ => panic!("Invalid CompressionMethod"),
        }
    }
}

#[derive(Debug, PartialEq)]
struct ClientHello {
    client_version: ProtocolVersion,
    random: Random,
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    legacy_compression_methods: Vec<CompressionMethod>,
    extensions: Vec<Extension>,
}

impl ClientHello {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, client_version) = ProtocolVersion::parse(input)?;
        let (input, random) = Random::parse(input)?;
        let (input, legacy_session_id) = take_while_m_n(0, 32, |c| c != 0)(input)?;
        let (input, cipher_suites_len) = be_u16(input)?;
        let (input, cipher_suites) = take(cipher_suites_len)(input)?;
        let mut cipher_suites: Vec<u16> = Vec::new();
        for chunk in cipher_suites.chunks_exact(2) {
            cipher_suites.push(u16::from_be_bytes([chunk[0].try_into().unwrap(), chunk[1]]));
        }
        let (input, legacy_compression_methods_len) = be_u8(input)?;
        let (input, legacy_compression_methods) = take(legacy_compression_methods_len)(input)?;
        let mut legacy_compression_methods: Vec<CompressionMethod> = Vec::new();
        for c in legacy_compression_methods {
            legacy_compression_methods.push(CompressionMethod::from(c));
        }
        let (input, extensions_len) = be_u16(input)?;
        let mut extensions_len = extensions_len as usize;
        let mut extensions: Vec<Extension> = Vec::new();
        while extensions_len > 0 {
            let (input, extension) = Extension::parse(input)?;
            extensions.push(extension);
            extensions_len -= 4 + extension.extension_data.len();
        }
        Ok((input, ClientHello {
            client_version,
            random,
            legacy_session_id: legacy_session_id.to_vec(),
            cipher_suites,
            legacy_compression_methods,
            extensions,
        }))
    }
}

#[derive(Debug, PartialEq)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

impl Extension {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, extension_type) = be_u16(input)?;
        let (input, extension_data_len) = be_u16(input)?;
        let (input, extension_data) = take(extension_data_len)(input)?;
        Ok((input, Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        }))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <binary_file>", args[0]);
    }
    let path = Path::new(&args[1]);
    let mut file = File::open(path).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");
    let (_input, client_hello) = ClientHello::parse(&buffer).unwrap();
    println!("{:?}", client_hello);
}