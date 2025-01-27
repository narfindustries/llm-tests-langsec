use nom::bytes::streaming::{tag, take};
use nom::combinator::{map, map_opt};
use nom::multi::{length_data, many0, many_m_n};
use nom::number::streaming::{be_u16, be_u8};
use nom::IResult;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::env;

#[derive(Debug)]
struct ClientHello {
    version: u16,
    random: [u8; 32],
    session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    data: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, version) = be_u16(input)?;
    let (input, random) = map_opt(take(32usize), |bytes: &[u8]| {
        let mut random = [0u8; 32];
        random.copy_from_slice(bytes);
        Some(random)
    })(input)?;
    let (input, session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites) = length_data(be_u16)(input)?;
    let cipher_suites = cipher_suites
        .chunks(2)
        .map(|chunk| u16::from_be_bytes([chunk[0], chunk[1]]))
        .collect();
    let (input, compression_methods) = length_data(be_u8)(input)?;
    let (input, extensions) = length_data(be_u16)(input)?;
    let (extensions, extensions) = many0(parse_extension)(extensions)?;

    Ok((
        input,
        ClientHello {
            version,
            random,
            session_id: session_id.to_vec(),
            cipher_suites,
            compression_methods: compression_methods.to_vec(),
            extensions,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, data) = length_data(be_u16)(input)?;
    Ok((
        input,
        Extension {
            extension_type,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(err) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", err);
        return;
    }

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => println!("{:?}", client_hello),
        Err(err) => eprintln!("Error parsing ClientHello: {:?}", err),
    }
}