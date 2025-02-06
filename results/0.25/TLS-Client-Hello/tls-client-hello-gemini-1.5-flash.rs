use nom::{
    bytes::complete::take,
    combinator::opt,
    multi::count,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::read;

#[derive(Debug)]
struct ClientHello {
    client_version: u16,
    random: [u8; 32],
    session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, client_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, session_id_len) = be_u8(input)?;
    let (input, session_id) = take(session_id_len as usize)(input)?;
    let (input, cipher_suites_len) = be_u16(input)?;
    let (input, cipher_suites) = count(cipher_suites_len as usize / 2, be_u16)(input)?;
    let (input, compression_methods_len) = be_u8(input)?;
    let (input, compression_methods) = take(compression_methods_len as usize)(input)?;
    let (input, extensions_len) = opt(be_u16)(input)?;
    let (input, extensions) = match extensions_len {
        Some(len) => {
            let (input, extensions) = parse_extensions(input, len as usize)?;
            (input, extensions)
        }
        None => (input, vec![]),
    };

    Ok((
        input,
        ClientHello {
            client_version,
            random: random.try_into().unwrap(),
            session_id: session_id.to_vec(),
            cipher_suites,
            compression_methods: compression_methods.to_vec(),
            extensions,
        },
    ))
}

fn parse_extensions(input: &[u8], len: usize) -> IResult<&[u8], Vec<Extension>> {
    let (input, extensions_bytes) = take(len)(input)?;
    let mut extensions = Vec::new();
    let mut remaining = extensions_bytes;
    while !remaining.is_empty() {
        let (rem, ext) = parse_extension(remaining)?;
        extensions.push(ext);
        remaining = rem;
    }
    Ok((input, extensions))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_len) = be_u16(input)?;
    let (input, extension_data) = take(extension_len as usize)(input)?;
    Ok((
        input,
        Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = match read(filename) {
        Ok(data) => data,
        Err(e) => {
            println!("Error reading file: {}", e);
            return;
        }
    };

    match parse_client_hello(&data) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => println!("Error parsing Client Hello: {:?}", e),
    }
}
