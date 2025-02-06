use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    multi::{length_data, many0},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct ClientHello {
    legacy_version: u16,
    random: Vec<u8>,
    legacy_session_id: Vec<u8>,
    cipher_suites: Vec<u16>,
    legacy_compression_methods: Vec<u8>,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, legacy_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, session_id) = length_data(be_u8)(input)?;
    let (input, cipher_suites) = length_data(be_u16)(input)?;
    let (input, cipher_suites) = many0(be_u16)(cipher_suites)?;
    let (input, legacy_compression_methods) = length_data(be_u8)(input)?;
    let (input, extensions) = length_data(be_u16)(input)?;
    let (input, extensions) = many0(parse_extension)(extensions)?;

    Ok((
        input,
        ClientHello {
            legacy_version,
            random: random.to_vec(),
            legacy_session_id: session_id.to_vec(),
            cipher_suites,
            legacy_compression_methods: legacy_compression_methods.to_vec(),
            extensions,
        },
    ))
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

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No file specified"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => {
            println!("Parsed Client Hello: {:?}", client_hello);
        }
        Err(e) => {
            println!("Error parsing Client Hello: {:?}", e);
        }
    }

    Ok(())
}