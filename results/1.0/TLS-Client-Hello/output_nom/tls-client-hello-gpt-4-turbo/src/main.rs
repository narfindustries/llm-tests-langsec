use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::many0,
    number::complete::{be_u16, be_u24, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct Random {
    gmt_unix_time: u32,
    random_bytes: Vec<u8>,
}

#[derive(Debug)]
struct SessionID {
    length: u8,
    value: Vec<u8>,
}

#[derive(Debug)]
struct CipherSuite(u16);

#[derive(Debug)]
struct CompressionMethod(u8);

#[derive(Debug)]
struct TLSClientHello {
    protocol_version: (u8, u8),
    random: Random,
    session_id: SessionID,
    cipher_suites: Vec<CipherSuite>,
    compression_methods: Vec<CompressionMethod>,
    extensions: Vec<Extension>,
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Random> {
    map(
        tuple((be_u32, take(28_usize))),
        |(gmt_unix_time, random_bytes)| Random {
            gmt_unix_time,
            random_bytes: random_bytes.to_vec(),
        },
    )(input)
}

fn parse_session_id(input: &[u8]) -> IResult<&[u8], SessionID> {
    let (input, length) = be_u8(input)?;
    map(take(length as usize), |value: &[u8]| SessionID {
        length,
        value: value.to_vec(),
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
    let (input, length) = be_u16(input)?;
    map(take(length as usize), move |data: &[u8]| Extension {
        extension_type,
        data: data.to_vec(),
    })(input)
}

fn parse_tls_client_hello(input: &[u8]) -> IResult<&[u8], TLSClientHello> {
    let (input, _) = tag(&[0x16])(input)?; // Content type: Handshake
    let (input, _) = be_u16(input)?; // Protocol version (TLS version)
    let (input, _) = be_u16(input)?; // Length
    let (input, _) = tag(&[0x01])(input)?; // Handshake type: ClientHello
    let (input, _) = be_u24(input)?; // Length

    let (input, protocol_version) = tuple((be_u8, be_u8))(input)?;
    let (input, random) = parse_random(input)?;
    let (input, session_id) = parse_session_id(input)?;
    let (input, cipher_suite_length) = be_u16(input)?;
    let (input, cipher_suites) = many0(parse_cipher_suite)(input)?;
    let (input, compression_methods_length) = be_u8(input)?;
    let (input, compression_methods) = many0(parse_compression_method)(input)?;
    let (input, extensions_length) = be_u16(input)?;
    let (input, extensions) = many0(parse_extension)(input)?;

    Ok((
        input,
        TLSClientHello {
            protocol_version,
            random,
            session_id,
            cipher_suites,
            compression_methods,
            extensions,
        },
    ))
}

fn main() -> Result<(), io::Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input file specified"));
    }
    let file_path = &args[1];
    let mut file = File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tls_client_hello(&buffer) {
        Ok((_, hello)) => println!("{:#?}", hello),
        Err(e) => println!("Failed to parse TLS Client Hello: {:?}", e),
    }

    Ok(())
}
