use nom::{
    bytes::complete::{tag, take, take_while, take_while1},
    combinator::{map, map_res, opt, value},
    multi::length_count,
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::read;
use std::net::IpAddr;
use std::path::Path;

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
    let parse_extension = |i: &[u8]| -> IResult<&[u8], Extension> {
        let (i, extension_type) = be_u16(i)?;
        let (i, extension_length) = be_u16(i)?;
        let (i, extension_data) = take(extension_length as usize)(i)?;

        match extension_type {
            0 => {
                // Server Name Indication
                let (i, server_name) = parse_server_name_indication(extension_data)?;
                Ok((i, Extension::ServerName(server_name)))
            }
            _ => Ok((i, Extension::Unknown(extension_data.to_vec()))),
        }
    };

    let (i, client_version) = tuple((be_u8, be_u8))(i)?;
    let (i, random) = take(32usize)(i)?;
    let (i, session_id) = opt(preceded(be_u8, length_count(be_u8, take(1))));
    let (i, cipher_suites) = length_count(be_u16, tuple((be_u8, be_u8)));
    let (i, compression_methods) = length_count(be_u8, be_u8);
    let (i, extensions) = opt(preceded(be_u16, length_count(be_u16, parse_extension)));

    Ok((
        i,
        ClientHello {
            client_version,
            random: random.try_into().unwrap(),
            session_id: session_id.map(|x| x.1),
            cipher_suites,
            compression_methods,
            extensions: extensions.unwrap_or_default(),
        },
    ))
}

fn parse_server_name_indication(input: &[u8]) -> IResult<&[u8], String> {
    let (i, name_type) = be_u8(i)?;
    assert_eq!(name_type, 0); // hostname
    let (i, server_name_length) = be_u16(i)?;
    let (i, server_name) = take(server_name_length as usize)(i)?;
    let server_name = String::from_utf8_lossy(server_name).to_string();
    Ok((i, server_name))
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let input = read(path)?;
    match parse_client_hello(&input) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Error parsing Client Hello: {:?}", e),
    }
    Ok(())
}
