use nom::{
    bytes::complete::{tag, take, take_while, take_while1},
    combinator::{map, map_res, opt, recognize},
    error::ErrorKind,
    multi::count,
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::net::Ipv4Addr;
use std::str::from_utf8;

#[derive(Debug)]
struct ClientHello {
    client_version: (u8, u8),
    random: [u8; 32],
    session_id: Option<Vec<u8>>,
    cipher_suites: Vec<(u8, u8)>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
enum Extension {
    ServerName(Vec<ServerName>),
    // Add other extensions as needed
    Unknown(Vec<u8>),
}

#[derive(Debug)]
struct ServerName {
    name_type: u8,
    host_name: String,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, client_version) = tuple((be_u8, be_u8))(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, session_id) = opt(preceded(be_u8, take_while(|b| b != 0)))(input)?;
    let (input, cipher_suites) = count(tuple((be_u8, be_u8)), be_u16(input)? as usize)(input)?;
    let (input, compression_methods) = count(be_u8, be_u8(input)? as usize)(input)?;
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


fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (mut input, extensions_length) = be_u16(input)?;
    let (mut input, extensions) = take(extensions_length as usize)(input)?;
    let mut parsed_extensions = Vec::new();
    while !extensions.is_empty(){
        let (rem, extension) = parse_extension(extensions)?;
        parsed_extensions.push(extension);
        extensions = rem;
    }
    Ok((input, parsed_extensions))
}


fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_length) = be_u16(input)?;
    let (input, extension_data) = take(extension_length as usize)(input)?;

    match extension_type {
        0 => {
            let (input, server_names) = parse_server_name(extension_data)?;
            Ok((input, Extension::ServerName(server_names)))
        }
        _ => Ok((input, Extension::Unknown(extension_data.to_vec()))),
    }
}

fn parse_server_name(input: &[u8]) -> IResult<&[u8], Vec<ServerName>> {
    let (input, list_length) = be_u16(input)?;
    let (input, server_names) = count(parse_server_name_entry, list_length as usize)(input)?;
    Ok((input, server_names))
}

fn parse_server_name_entry(input: &[u8]) -> IResult<&[u8], ServerName> {
    let (input, name_type) = be_u8(input)?;
    let (input, name_length) = be_u16(input)?;
    let (input, name) = take(name_length as usize)(input)?;
    let host_name = from_utf8(name).map_err(|_| nom::Err::Error((name, ErrorKind::Verify)))?;
    Ok((input, ServerName { name_type, host_name: host_name.to_string() }))
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((remaining, client_hello)) => {
            println!("Parsed Client Hello:\n{:?}\nRemaining: {:?}", client_hello, remaining);
        }
        Err(e) => {
            println!("Error parsing Client Hello: {:?}", e);
        }
    }

    Ok(())
}

