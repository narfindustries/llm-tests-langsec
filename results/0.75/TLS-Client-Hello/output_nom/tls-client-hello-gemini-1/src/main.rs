use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, opt, all_consuming},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::fs;
use std::path::Path;

#[derive(Debug)]
struct ClientHello {
    protocol_version: (u8, u8),
    random: Vec<u8>,
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
    let (rem, protocol_version) = tuple((be_u8, be_u8))(input)?;
    let (rem, random) = take(32usize)(rem)?;
    let (rem, session_id_len) = be_u8(rem)?;
    let (rem, session_id) = take(session_id_len as usize)(rem)?;
    let (rem, cipher_suites_len) = be_u16(rem)?;
    let (rem, cipher_suites_bytes) = take(cipher_suites_len as usize)(rem)?;
    let cipher_suites: Vec<u16> = cipher_suites_bytes
        .chunks(2)
        .map(|chunk| u16::from_be_bytes(chunk.try_into().unwrap()))
        .collect();

    let (rem, compression_methods_len) = be_u8(rem)?;
    let (rem, compression_methods) = take(compression_methods_len as usize)(rem)?;
    let (rem, extensions_len) = opt(be_u16)(rem)?;
    let extensions = match extensions_len {
        Some(len) => {
            let (rem, extensions_bytes) = take(len as usize)(rem)?;
            let mut extensions = Vec::new();
            let mut remaining_extensions = extensions_bytes;
            while !remaining_extensions.is_empty() {
                let (rem_ext, extension) = parse_extension(remaining_extensions)?;
                extensions.push(extension);
                remaining_extensions = rem_ext;
            }
            extensions
        }
        None => Vec::new(),
    };

    Ok((
        rem,
        ClientHello {
            protocol_version,
            random: random.to_vec(),
            session_id: session_id.to_vec(),
            cipher_suites,
            compression_methods: compression_methods.to_vec(),
            extensions,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (rem, extension_type) = be_u16(input)?;
    let (rem, extension_data_len) = be_u16(rem)?;
    let (rem, extension_data) = take(extension_data_len as usize)(rem)?;
    Ok((
        rem,
        Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let input = match fs::read(path) {
        Ok(input) => input,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            std::process::exit(1);
        }
    };

    let result = all_consuming(parse_client_hello)(&input);
    match result {
        Ok((rem, client_hello)) => {
            println!("Parsed Client Hello:\n{:?}", client_hello);
            if !rem.is_empty() {
                println!("Remaining bytes: {:?}", rem);
            }
        }
        Err(e) => {
            eprintln!("Error parsing Client Hello: {:?}", e);
            std::process::exit(1);
        }
    };
}
