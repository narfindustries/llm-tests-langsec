use nom::{
    bytes::complete::{take, take_as},
    combinator::{map, opt},
    error::{context, ErrorKind, ParseError},
    multi::{length_data, take_while_m_n},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug, PartialEq)]
enum TlsVersion {
    V1_0,
    V1_1,
    V1_2,
    V1_3,
}

impl TlsVersion {
    fn parse(input: &[u8]) -> IResult<&[u8], TlsVersion> {
        context(
            "TLS version",
            map(be_u16, |version: u16| match version {
                0x0300 => TlsVersion::V1_0,
                0x0301 => TlsVersion::V1_1,
                0x0302 => TlsVersion::V1_2,
                0x0303 => TlsVersion::V1_2,
                0x0304 => TlsVersion::V1_3,
                _ => unreachable!("Invalid TLS version"),
            }),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct Random {
    bytes: Vec<u8>,
}

impl Random {
    fn parse(input: &[u8]) -> IResult<&[u8], Random> {
        context(
            "random bytes",
            map(take(32usize), |bytes: Vec<u8>| Random { bytes }),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct LegacySessionId {
    session_id: Vec<u8>,
}

impl LegacySessionId {
    fn parse(input: &[u8]) -> IResult<&[u8], LegacySessionId> {
        context(
            "legacy session id",
            map(length_data(be_u8), |session_id: Vec<u8>| LegacySessionId { session_id },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct CipherSuite {
    id: u16,
}

impl CipherSuite {
    fn parse(input: &[u8]) -> IResult<&[u8], CipherSuite> {
        context(
            "cipher suite",
            map(be_u16, |id: u16| CipherSuite { id }),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct CipherSuites {
    cipher_suites: Vec<CipherSuite>,
}

impl CipherSuites {
    fn parse(input: &[u8]) -> IResult<&[u8], CipherSuites> {
        context(
            "cipher suites",
            map(length_data(be_u16), |cipher_suites: Vec<u8>| {
                CipherSuites {
                    cipher_suites: cipher_suites
                        .chunks(2)
                        .map(|chunk| CipherSuite {
                            id: ((chunk[0] as u16) << 8) | (chunk[1] as u16),
                        })
                        .collect(),
                }
            }),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct CompressionMethod {
    id: u8,
}

impl CompressionMethod {
    fn parse(input: &[u8]) -> IResult<&[u8], CompressionMethod> {
        context(
            "compression method",
            map(be_u8, |id: u8| CompressionMethod { id }),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct CompressionMethods {
    compression_methods: Vec<CompressionMethod>,
}

impl CompressionMethods {
    fn parse(input: &[u8]) -> IResult<&[u8], CompressionMethods> {
        context(
            "compression methods",
            map(length_data(be_u8), |compression_methods: Vec<u8>| {
                CompressionMethods {
                    compression_methods: compression_methods
                        .into_iter()
                        .map(|x| CompressionMethod::parse(xinto)
                        .collect(),
                }
            }),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
enum ExtensionType {
    SupportedVersions,
    SupportedGroups,
    SignatureAlgorithms,
    KeyShare,
    PSKKeyExchangeModes,
    EarlyData,
    PreSharedKey,
    Cookie,
}

impl ExtensionType {
    fn parse(input: &[u8]) -> IResult<&[u8], ExtensionType> {
        context(
            "extension type",
            map(be_u16, |id: u16| match id {
                0x002b => ExtensionType::SupportedVersions,
                0x000a => ExtensionType::SupportedGroups,
                0x000d => ExtensionType::SignatureAlgorithms,
                0x0033 => ExtensionType::KeyShare,
                0x002d => ExtensionType::PSKKeyExchangeModes,
                0x0026 => ExtensionType::EarlyData,
                0x0029 => ExtensionType::PreSharedKey,
                _ => unreachable!("Invalid extension type"),
            }),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct Extension {
    ext_type: ExtensionType,
    data: Vec<u8>,
}

impl Extension {
    fn parse(input: &[u8]) -> IResult<&[u8], Extension> {
        context(
            "extension",
            map(tuple((ExtensionType::parse, length_data(be_u16)), |(ext_type, data): (ExtensionType, Vec<u8>)| {
                Extension { ext_type, data }
            }),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct Extensions {
    extensions: Vec<Extension>,
}

impl Extensions {
    fn parse(input: &[u8]) -> IResult<&[u8], Extensions> {
        context(
            "extensions",
            map(length_data(be_u16), |extensions: Vec<u8>| Extensions {
                extensions: extensions
                    .split(|&x| x == 0)
                    .filter(|x| x.len() > 0)
                    .map(|ext| Extension::parse(xinto)
                    .collect(),
            }),
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct ClientHello {
    legacy_version: TlsVersion,
    random: Random,
    legacy_session_id: LegacySessionId,
    cipher_suites: CipherSuites,
    compression_methods: CompressionMethods,
    extensions: Extensions,
}

impl ClientHello {
    fn parse(input: &[u8]) -> IResult<&[u8], ClientHello> {
        context(
            "client hello",
            map(
                tuple(
                    (
                        TlsVersion::parse,
                        Random::parse,
                        LegacySessionId::parse,
                        CipherSuites::parse,
                        CompressionMethods::parse,
                        Extensions::parse,
                    ),
                ),
            ),
        )(input)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let path = Path::new(&args[1]);
    let file = File::open(path).unwrap();
    let reader = BufReader::new(file);
    let mut buf = Vec::new();
    reader.read_to_end(&mut buf).unwrap();
    let client_hello = ClientHello::parse(&buf).unwrap();
    println!("{:?}", client_hello);
}