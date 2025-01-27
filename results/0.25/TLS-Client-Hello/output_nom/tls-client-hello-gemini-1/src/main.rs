use nom::{
    bytes::complete::{tag, take, take_while, take_while1},
    combinator::{map, map_res, opt, rest},
    error::ErrorKind,
    multi::length_count,
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::read;
use std::net::IpAddr;
use std::str;

#[derive(Debug)]
enum Extension {
    ServerName(String),
    // Add other extensions as needed
    Unknown(Vec<u8>),
}

#[derive(Debug)]
struct ClientHello {
    client_version: (u8, u8),
    random: [u8; 32],
    session_id: Option<Vec<u8>>,
    cipher_suites: Vec<(u8, u8)>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, client_version) = tuple((be_u8, be_u8))(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, session_id) = length_count(be_u8, take)(input)?;
    let (input, cipher_suites) = length_count(be_u16, tuple((be_u8, be_u8)))(input)?;
    let (input, compression_methods) = length_count(be_u8, be_u8)(input)?;
    let (input, extensions) = opt(parse_extensions)(input)?;

    Ok((
        input,
        ClientHello {
            client_version,
            random: random.try_into().unwrap(),
            session_id: session_id.map(|x| x.to_vec()),
            cipher_suites,
            compression_methods,
            extensions: extensions.unwrap_or_default(),
        },
    ))
}


fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_len) = be_u16(input)?;
    let (input, extension_data) = take(extension_len as usize)(input)?;

    match extension_type {
        0 => {
            let (data, server_name) = parse_server_name(extension_data)?;
            Ok((input, Extension::ServerName(server_name)))
        }
        _ => Ok((input, Extension::Unknown(extension_data.to_vec()))),
    }
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    length_count(be_u16, parse_extension)(input)
}

fn parse_server_name(input: &[u8]) -> IResult<&[u8], String> {
    let (input, _) = tag(&[0, 0])(input)?; // Server Name Type
    let (input, server_name_len) = be_u16(input)?;
    let (input, server_name) = take(server_name_len as usize)(input)?;
    let server_name_str = match str::from_utf8(server_name) {
        Ok(s) => s.to_string(),
        Err(_) => return Err(nom::Err::Error((server_name, ErrorKind::Verify))),
    };
    Ok((input, server_name_str))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let data = match read(filename) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            std::process::exit(1);
        }
    };

    match parse_client_hello(&data) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => {
            eprintln!("Error parsing Client Hello: {:?}", e);
            std::process::exit(1);
        }
    }
}
