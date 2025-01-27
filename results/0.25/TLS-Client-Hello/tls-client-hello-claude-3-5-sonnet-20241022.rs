use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct ClientHello {
    version: (u8, u8),
    random: Random,
    session_id: Option<Vec<u8>>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Random {
    gmt_unix_time: u32,
    random_bytes: [u8; 28],
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Random> {
    let (input, gmt_unix_time) = be_u32(input)?;
    let (input, random_bytes) = map(take(28usize), |bytes: &[u8]| {
        let mut arr = [0u8; 28];
        arr.copy_from_slice(bytes);
        arr
    })(input)?;
    Ok((input, Random { gmt_unix_time, random_bytes }))
}

fn parse_session_id(input: &[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    let (input, length) = be_u8(input)?;
    if length == 0 {
        Ok((input, None))
    } else {
        let (input, session_id) = take(length as usize)(input)?;
        Ok((input, Some(session_id.to_vec())))
    }
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, length) = be_u16(input)?;
    let (input, cipher_suites) = many0(be_u16)(take(length as usize)(input)?.0)?;
    Ok((input, cipher_suites))
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u8(input)?;
    let (input, compression_methods) = take(length as usize)(input)?;
    Ok((input, compression_methods.to_vec()))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_data) = length_data(be_u16)(input)?;
    Ok((input, Extension {
        extension_type,
        extension_data: extension_data.to_vec(),
    }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (input, length) = be_u16(input)?;
    let (input, extensions) = many0(parse_extension)(take(length as usize)(input)?.0)?;
    Ok((input, extensions))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _) = tag(&[0x16])(input)?; // Content Type: Handshake
    let (input, _) = tag(&[0x03, 0x03])(input)?; // TLS Version 1.2
    let (input, length) = be_u16(input)?;
    let (input, _) = tag(&[0x01])(input)?; // Handshake Type: Client Hello
    let (input, _) = be_u24(input)?; // Length
    let (input, version) = pair(be_u8, be_u8)(input)?;
    let (input, random) = parse_random(input)?;
    let (input, session_id) = parse_session_id(input)?;
    let (input, cipher_suites) = parse_cipher_suites(input)?;
    let (input, compression_methods) = parse_compression_methods(input)?;
    let (input, extensions) = parse_extensions(input)?;

    Ok((input, ClientHello {
        version,
        random,
        session_id,
        cipher_suites,
        compression_methods,
        extensions,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((remaining, client_hello)) => {
            println!("Parsed Client Hello: {:#?}", client_hello);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => eprintln!("Failed to parse Client Hello: {:?}", e),
    }

    Ok(())
}