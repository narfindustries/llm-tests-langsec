use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

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
    map(
        tuple((be_u32, take(28usize))),
        |(gmt_unix_time, random_bytes): (u32, &[u8])| Random {
            gmt_unix_time,
            random_bytes: random_bytes.to_vec(),
        },
    )(input)
}

fn parse_session_id(input: &[u8]) -> IResult<&[u8], SessionID> {
    let (input, length) = be_u8(input)?;
    map(take(length), move |session_id: &[u8]| SessionID {
        length,
        session_id: session_id.to_vec(),
    })(input)
}

fn parse_cipher_suite(input: &[u8]) -> IResult<&[u8], CipherSuite> {
    map(be_u16, CipherSuite)(input)
}

fn parse_compression_method(input: &[u8]) -> IResult<&[u8], CompressionMethod> {
    map(be_u8, CompressionMethod)(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_data) = length_data(be_u16)(input)?;
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
    let (input, _) = take(2usize)(input)?; // Version: TLS 1.x
    let (input, _) = be_u24(input)?; // Length
    let (input, _) = tag(&[0x01])(input)?; // Client Hello
    let (input, _) = be_u24(input)?; // Length
    let (input, protocol_version) = tuple((be_u8, be_u8))(input)?;
    let (input, random) = parse_random(input)?;
    let (input, session_id) = parse_session_id(input)?;
    let (input, cipher_suites) = preceded(be_u16, many0(parse_cipher_suite))(input)?;
    let (input, compression_methods) = preceded(be_u8, many0(parse_compression_method))(input)?;
    let (input, extensions) = preceded(be_u16, many0(parse_extension))(input)?;

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
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => {
            println!("{:#?}", client_hello);
        }
        Err(e) => {
            eprintln!("Failed to parse Client Hello: {:?}", e);
        }
    }

    Ok(())
}