use clap::Parser;
use nom::{
    bits::complete::take,
    bytes::complete::{take as take_bytes, take_while},
    combinator::map,
    multi::length_count,
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::fs;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    input: String,
}

#[derive(Debug)]
struct ClientHello {
    version: u16,
    random: Random,
    session_id: Vec<u8>,
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
    let (input, random_bytes) = map(take_bytes(28usize), |bytes: &[u8]| {
        let mut arr = [0u8; 28];
        arr.copy_from_slice(bytes);
        arr
    })(input)?;

    Ok((
        input,
        Random {
            gmt_unix_time,
            random_bytes,
        },
    ))
}

fn parse_session_id(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u8(input)?;
    let (input, session_id) = take_bytes(length as usize)(input)?;
    Ok((input, session_id.to_vec()))
}

fn parse_cipher_suites(input: &[u8]) -> IResult<&[u8], Vec<u16>> {
    let (input, length) = be_u16(input)?;
    let (input, cipher_suites) = length_count(map(be_u16, |_| 1u8), be_u16)(input)?;
    Ok((input, cipher_suites))
}

fn parse_compression_methods(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u8(input)?;
    let (input, compression_methods) = take_bytes(length as usize)(input)?;
    Ok((input, compression_methods.to_vec()))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = be_u16(input)?;
    let (input, extension_length) = be_u16(input)?;
    let (input, extension_data) = take_bytes(extension_length as usize)(input)?;

    Ok((
        input,
        Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    ))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<Extension>> {
    let (input, extensions_length) = be_u16(input)?;
    let (input, extensions) = length_count(map(be_u16, |_| 1u8), parse_extension)(input)?;
    Ok((input, extensions))
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, version) = be_u16(input)?;
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

fn parse_tls_record(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _content_type) = be_u8(input)?;
    let (input, _protocol_version) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, _handshake_type) = be_u8(input)?;
    let (input, _handshake_length) = be_u24(input)?;
    let (input, client_hello) = parse_client_hello(input)?;

    Ok((input, client_hello))
}

fn main() {
    let args = Args::parse();
    let data = fs::read(args.input).expect("Unable to read file");
    
    match parse_tls_record(&data) {
        Ok((remaining, client_hello)) => {
            println!("Parsed ClientHello: {:?}", client_hello);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => println!("Error parsing TLS record: {:?}", e),
    }
}