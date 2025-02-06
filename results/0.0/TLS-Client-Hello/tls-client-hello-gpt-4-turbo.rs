use nom::{
    bytes::complete::{take, take_while1},
    combinator::{map, map_res},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

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
    ServerName(String),
    SupportedGroups(Vec<u16>),
    SignatureAlgorithms(Vec<u16>),
    Other(u16, Vec<u8>),
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, legacy_session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites) = preceded(be_u16, many0(be_u16))(input)?;
    let (input, legacy_compression_methods) = length_data(be_u8)(input)?;
    let (input, extensions) = many0(parse_extension)(input)?;

    Ok((
        input,
        ClientHello {
            legacy_version,
            random: random.to_vec(),
            legacy_session_id: legacy_session_id.to_vec(),
            cipher_suites,
            legacy_compression_methods: legacy_compression_methods.to_vec(),
            extensions,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, (ext_type, ext_data)) = tuple((be_u16, length_data(be_u16)))(input)?;
    match ext_type {
        0 => {
            let (rest, server_name) = map_res(take_while1(|c: u8| c != 0), std::str::from_utf8)(ext_data)?;
            Ok((input, Extension::ServerName(server_name.to_string())))
        }
        10 => {
            let (rest, supported_groups) = many0(be_u16)(ext_data)?;
            Ok((input, Extension::SupportedGroups(supported_groups)))
        }
        13 => {
            let (rest, signature_algorithms) = many0(be_u16)(ext_data)?;
            Ok((input, Extension::SignatureAlgorithms(signature_algorithms)))
        }
        _ => Ok((input, Extension::Other(ext_type, ext_data.to_vec()))),
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Failed to parse Client Hello: {:?}", e),
    }

    Ok(())
}