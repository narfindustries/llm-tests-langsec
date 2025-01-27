use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::length_data,
    number::complete::{be_u16, be_u24, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::{env, fs, io::Read};

#[derive(Debug)]
struct Random([u8; 32]);

#[derive(Debug)]
struct SessionID(Vec<u8>);

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
    version: (u8, u8),
    random: Random,
    session_id: SessionID,
    cipher_suites: Vec<CipherSuite>,
    compression_methods: Vec<CompressionMethod>,
    extensions: Vec<Extension>,
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Random> {
    map(take(32usize), |bytes: &[u8]| {
        let mut random = [0u8; 32];
        random.copy_from_slice(bytes);
        Random(random)
    })(input)
}

fn parse_session_id(input: &[u8]) -> IResult<&[u8], SessionID> {
    map_res(length_data(be_u8), |bytes: &[u8]| {
        Ok::<_, ()>(SessionID(bytes.to_vec()))
    })(input)
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<CipherSuite>> {
    map_res(length_data(be_u16), |bytes: &[u8]| {
        bytes
            .chunks_exact(2)
            .map(|chunk| Ok(CipherSuite(u16::from_be_bytes([chunk[0], chunk[1]]))))
            .collect::<Result<Vec<_>, ()>>()
    })(input)
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<CompressionMethod>> {
    map_res(length_data(be_u8), |bytes: &[u8]| {
        bytes
            .iter()
            .map(|&b| Ok(CompressionMethod(b)))
            .collect::<Result<Vec<_>, ()>>()
    })(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    map(
        tuple((be_u16, length_data(be_u16))),
        |(extension_type, extension_data)| Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    )(input)
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    map_res(length_data(be_u16), |bytes: &[u8]| {
        let mut extensions = Vec::new();
        let mut remaining = bytes;
        while !remaining.is_empty() {
            let (rest, extension) = parse_extension(remaining)?;
            extensions.push(extension);
            remaining = rest;
        }
        Ok::<_, ()>(extensions)
    })(input)
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _) = tag([0x16])(input)?;
    let (input, _) = be_u16(input)?;
    let (input, _) = tag([0x01])(input)?;
    let (input, _) = be_u24(input)?;
    let (input, version) = pair(be_u8, be_u8)(input)?;
    let (input, random) = parse_random(input)?;
    let (input, session_id) = parse_session_id(input)?;
    let (input, cipher_suites) = parse_cipher_suites(input)?;
    let (input, compression_methods) = parse_compression_methods(input)?;
    let (input, extensions) = parse_extensions(input)?;

    Ok((
        input,
        ClientHello {
            version,
            random,
            session_id,
            cipher_suites,
            compression_methods,
            extensions,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let mut file = fs::File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Failed to parse ClientHello: {:?}", e),
    }
}