use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{count, many0},
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct Random {
    gmt_unix_time: u32,
    random_bytes: Vec<u8>,
}

#[derive(Debug)]
struct SessionID {
    length: u8,
    session_id: Vec<u8>,
}

#[derive(Debug)]
struct CipherSuite(u16);

#[derive(Debug)]
struct CompressionMethod(u8);

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

#[derive(Debug)]
struct ClientHello {
    protocol_version: (u8, u8),
    random: Random,
    session_id: SessionID,
    cipher_suites: Vec<CipherSuite>,
    compression_methods: Vec<CompressionMethod>,
    extensions: Vec<Extension>,
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Random> {
    let (input, gmt_unix_time) = be_u32(input)?;
    let (input, random_bytes) = take(28u8)(input)?;
    Ok((
        input,
        Random {
            gmt_unix_time,
            random_bytes: random_bytes.to_vec(),
        },
    ))
}

fn parse_session_id(input: &[u8]) -> IResult<&[u8], SessionID> {
    let (input, length) = be_u8(input)?;
    let (input, session_id) = take(length)(input)?;
    Ok((
        input,
        SessionID {
            length,
            session_id: session_id.to_vec(),
        },
    ))
}

fn parse_cipher_suite(input: &[u8]) -> IResult<&[u8], CipherSuite> {
    map(be_u16, CipherSuite)(input)
}

fn parse_compression_method(input: &[u8]) -> IResult<&[u8], CompressionMethod> {
    map(be_u8, CompressionMethod)(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, extension_data) = take(length)(input)?;
    Ok((
        input,
        Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    ))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _) = tag(&[0x16])(input)?; // Handshake type: Handshake
    let (input, _) = take(2u8)(input)?; // Version (TLS 1.0 or higher)
    let (input, _) = be_u24(input)?; // Length
    let (input, _) = tag(&[0x01])(input)?; // ClientHello
    let (input, _) = be_u24(input)?; // Length
    let (input, protocol_version) = tuple((be_u8, be_u8))(input)?;
    let (input, random) = parse_random(input)?;
    let (input, session_id) = parse_session_id(input)?;
    let (input, cipher_suites_length) = be_u16(input)?;
    let (input, cipher_suites) = count(parse_cipher_suite, (cipher_suites_length / 2) as usize)(input)?;
    let (input, compression_methods_length) = be_u8(input)?;
    let (input, compression_methods) = count(parse_compression_method, compression_methods_length as usize)(input)?;
    let (input, extensions_length) = be_u16(input)?;
    let (input, extensions) = many0(parse_extension)(input)?;
    Ok((
        input,
        ClientHello {
            protocol_version,
            random,
            session_id,
            cipher_suites,
            compression_methods,
            extensions,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Failed to parse ClientHello: {:?}", e),
    }

    Ok(())
}