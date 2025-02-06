use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult, multi::many0,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

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
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_u8_length_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u8(input)?;
    let (input, data) = take(length)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_u16_length_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, (extension_type, extension_data)) = tuple((be_u16, parse_u16_length_data))(input)?;
    Ok((input, Extension { extension_type, extension_data }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    many0(parse_extension)(input)
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, legacy_session_id) = parse_u8_length_data(input)?;
    let (input, cipher_suites) = preceded(be_u16, many0(be_u16))(input)?;
    let (input, legacy_compression_methods) = parse_u8_length_data(input)?;
    let (input, extensions) = parse_extensions(input)?;

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
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Usage: tls_parse <file_path>"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_rem, client_hello)) => {
            println!("Parsed ClientHello: {:?}", client_hello);
        }
        Err(e) => {
            println!("Failed to parse ClientHello: {:?}", e);
        }
    }

    Ok(())
}