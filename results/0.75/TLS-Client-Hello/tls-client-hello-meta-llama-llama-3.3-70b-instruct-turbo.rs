use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data, take_till},
    number::complete::{be_u16, be_u24, be_u8},
    sequence::{delimited, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
struct RandomBytes {
    gmt_unix_time: u32,
    random_bytes: Vec<u8>,
}

impl RandomBytes {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, gmt_unix_time) = be_u32(input)?;
        let (input, random_bytes) = take(28)(input)?;
        Ok((input, Self { gmt_unix_time, random_bytes: random_bytes.to_vec() }))
    }
}

#[derive(Debug)]
struct SessionId {
    length: u8,
    session_id: Vec<u8>,
}

impl SessionId {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, length) = be_u8(input)?;
        let (input, session_id) = take(length as usize)(input)?;
        Ok((input, Self { length, session_id: session_id.to_vec() }))
    }
}

#[derive(Debug)]
struct CipherSuite {
    value: u16,
}

impl CipherSuite {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, value) = be_u16(input)?;
        Ok((input, Self { value }))
    }
}

#[derive(Debug)]
struct CompressionMethod {
    value: u8,
}

impl CompressionMethod {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, value) = be_u8(input)?;
        Ok((input, Self { value }))
    }
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

impl Extension {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, extension_type) = be_u16(input)?;
        let (input, length) = be_u16(input)?;
        let (input, extension_data) = take(length as usize)(input)?;
        Ok((input, Self { extension_type, extension_data: extension_data.to_vec() }))
    }
}

#[derive(Debug)]
struct ClientHello {
    record_layer_version: (u8, u8),
    record_layer_length: u16,
    handshake_type: u8,
    handshake_length: u24,
    handshake_version: (u8, u8),
    random: RandomBytes,
    session_id_length: u8,
    session_id: SessionId,
    cipher_suites_length: u16,
    cipher_suites: Vec<CipherSuite>,
    compression_methods_length: u8,
    compression_methods: Vec<CompressionMethod>,
    extensions_length: u16,
    extensions: Vec<Extension>,
}

impl ClientHello {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, record_layer_version) = tuple((be_u8, be_u8))(input)?;
        let (input, record_layer_length) = be_u16(input)?;
        let (input, handshake_type) = be_u8(input)?;
        let (input, handshake_length) = be_u24(input)?;
        let (input, handshake_version) = tuple((be_u8, be_u8))(input)?;
        let (input, random) = RandomBytes::parse(input)?;
        let (input, session_id_length) = be_u8(input)?;
        let (input, session_id) = SessionId::parse(input)?;
        let (input, cipher_suites_length) = be_u16(input)?;
        let (input, cipher_suites) = length_data(be_u16)(input)?;
        let mut cipher_suites = Vec::new();
        let mut remaining = cipher_suites.1;
        while !remaining.is_empty() {
            let (next, cs) = CipherSuite::parse(remaining)?;
            cipher_suites.push(cs);
            remaining = next;
        }
        let (input, compression_methods_length) = be_u8(input)?;
        let (input, compression_methods) = length_data(be_u8)(input)?;
        let mut compression_methods = Vec::new();
        let mut remaining = compression_methods.1;
        while !remaining.is_empty() {
            let (next, cm) = CompressionMethod::parse(remaining)?;
            compression_methods.push(cm);
            remaining = next;
        }
        let (input, extensions_length) = be_u16(input)?;
        let (input, extensions) = length_data(be_u16)(input)?;
        let mut extensions = Vec::new();
        let mut remaining = extensions.1;
        while !remaining.is_empty() {
            let (next, ext) = Extension::parse(remaining)?;
            extensions.push(ext);
            remaining = next;
        }
        Ok((
            input,
            Self {
                record_layer_version,
                record_layer_length,
                handshake_type,
                handshake_length,
                handshake_version,
                random,
                session_id_length,
                session_id,
                cipher_suites_length,
                cipher_suites,
                compression_methods_length,
                compression_methods,
                extensions_length,
                extensions,
            },
        ))
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let file = File::open(&args[1])?;
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input)?;
    let (_remaining, client_hello) = ClientHello::parse(&input).unwrap();
    println!("{:?}", client_hello);
    Ok(())
}