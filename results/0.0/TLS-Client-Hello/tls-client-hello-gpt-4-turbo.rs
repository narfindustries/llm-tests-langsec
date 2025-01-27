use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u24, be_u8},
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
    id: Vec<u8>,
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
    map(length_data(be_u8), |id: &[u8]| SessionID {
        id: id.to_vec(),
    })(input)
}

fn parse_cipher_suite(input: &[u8]) -> IResult<&[u8], CipherSuite> {
    map(be_u16, CipherSuite)(input)
}

fn parse_compression_method(input: &[u8]) -> IResult<&[u8], CompressionMethod> {
    map(be_u8, CompressionMethod)(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    map(
        tuple((be_u16, length_data(be_u16))),
        |(extension_type, extension_data): (u16, &[u8])| Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    )(input)
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    map(
        tuple((
            tag(&[0x03, 0x01]), // Protocol version: TLS 1.0
            parse_random,
            parse_session_id,
            preceded(be_u16, many0(parse_cipher_suite)),
            preceded(be_u8, many0(parse_compression_method)),
            preceded(be_u16, many0(parse_extension)),
        )),
        |(_, random, session_id, cipher_suites, compression_methods, extensions)| ClientHello {
            protocol_version: (3, 1),
            random,
            session_id,
            cipher_suites,
            compression_methods,
            extensions,
        },
    )(input)
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
        Err(e) => eprintln!("Failed to parse Client Hello: {:?}", e),
    }

    Ok(())
}