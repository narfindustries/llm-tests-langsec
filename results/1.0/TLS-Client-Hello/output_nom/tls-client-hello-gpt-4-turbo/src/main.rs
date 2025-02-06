use nom::{
    bytes::complete::take,
    multi::{many0, many_m_n},
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::convert::TryInto;

#[derive(Debug)]
struct ClientHello {
    legacy_version: u16,
    random: Vec<u8>,
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    legacy_compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
enum Extension {
    ServerName(Vec<u8>),
    SupportedGroups(Vec<u16>),
    SignatureAlgorithms(Vec<u16>),
    KeyShare(u8, Vec<u8>),
    PreSharedKey(u16),
    PskKeyExchangeModes(Vec<u8>),
    EarlyData,
    Unknown(u16, Vec<u8>),
}

fn parse_u8_length_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u8(input)?;
    take(length)(input).map(|(i, bytes)| (i, bytes.to_vec()))
}

fn parse_u16_length_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u16(input)?;
    take(length)(input).map(|(i, bytes)| (i, bytes.to_vec()))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, (ext_type, ext_data)) = tuple((be_u16, parse_u16_length_data))(input)?;
    match ext_type {
        0 => Ok((input, Extension::ServerName(ext_data))),
        10 => {
            let parsed = many_m_n(0, ext_data.len() / 2, be_u16)(&ext_data)?;
            Ok((input, Extension::SupportedGroups(parsed.1)))
        },
        13 => {
            let parsed = many_m_n(0, ext_data.len() / 2, be_u16)(&ext_data)?;
            Ok((input, Extension::SignatureAlgorithms(parsed.1)))
        },
        51 => {
            let (remainder, group) = be_u16(&ext_data)?;
            let key_exchange = parse_u16_length_data(&remainder[2..])?;
            Ok((input, Extension::KeyShare(group.try_into().expect("Group value too large"), key_exchange.1)))
        },
        41 => {
            let (_, psk) = be_u16(&ext_data)?;
            Ok((input, Extension::PreSharedKey(psk)))
        },
        45 => Ok((input, Extension::PskKeyExchangeModes(ext_data))),
        42 => Ok((input, Extension::EarlyData)),
        _ => Ok((input, Extension::Unknown(ext_type, ext_data))),
    }
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, legacy_session_id) = parse_u8_length_data(input)?;
    let (input, cipher_suites) = preceded(be_u16, many0(be_u16))(input)?;
    let (input, legacy_compression_methods) = parse_u8_length_data(input)?;
    let (input, extensions) = preceded(be_u16, many0(parse_extension))(input)?;

    Ok((
        input,
        ClientHello {
            legacy_version,
            random: random.to_vec(),
            legacy_session_id,
            cipher_suites,
            legacy_compression_methods,
            extensions,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Please provide exactly one argument which is the file path",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => println!("Failed to parse TLS Client Hello: {:?}", e),
    }

    Ok(())
}