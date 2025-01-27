use nom::{
    bytes::complete::{tag, take},
    combinator::{cond, map},
    multi::{count, many0},
    number::complete::{be_u16, be_u24, be_u8},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {
    #[structopt(parse(from_os_str))]
    input_file: PathBuf,
}

#[derive(Debug)]
struct Random {
    gmt_unix_time: u32,
    random_bytes: Vec<u8>,
}

#[derive(Debug)]
struct SessionID {
    length: u8,
    session_id: Vec<u8>,
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
    protocol_version: u16,
    random: Random,
    session_id: SessionID,
    cipher_suites: Vec<CipherSuite>,
    compression_methods: Vec<CompressionMethod>,
    extensions: Vec<Extension>,
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Random> {
    map(tuple((be_u32, take(28u8))), |(time, bytes)| Random {
        gmt_unix_time: time,
        random_bytes: bytes.to_vec(),
    })(input)
}

fn parse_session_id(input: &[u8]) -> IResult<&[u8], SessionID> {
    let (input, length) = be_u8(input)?;
    map(take(length), |bytes: &[u8]| SessionID {
        length,
        session_id: bytes.to_vec(),
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
    map(take(length), |bytes: &[u8]| Extension {
        extension_type,
        extension_data: bytes.to_vec(),
    })(input)
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, _) = tag(&[0x16])(input)?; // Handshake type: ClientHello
    let (input, _) = take(2u8)(input)?; // Protocol version (TLS)
    let (input, _) = be_u24(input)?; // Length of ClientHello
    let (input, protocol_version) = be_u16(input)?;
    let (input, random) = parse_random(input)?;
    let (input, session_id) = parse_session_id(input)?;
    let (input, cipher_suites_length) = be_u16(input)?;
    let (input, cipher_suites) = count(parse_cipher_suite, (cipher_suites_length / 2) as usize)(input)?;
    let (input, compression_methods_length) = be_u8(input)?;
    let (input, compression_methods) = count(parse_compression_method, compression_methods_length as usize)(input)?;
    let (input, extensions_length) = cond(input.len() > 0, be_u16)(input)?;
    let (input, extensions) = many0(parse_extension)(input)?;
    Ok((input, ClientHello {
        protocol_version,
        random,
        session_id,
        cipher_suites,
        compression_methods,
        extensions: extensions.unwrap_or_default(),
    }))
}

fn main() -> io::Result<()> {
    let opt = Opt::from_args();
    let mut file = File::open(opt.input_file)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, hello)) => println!("{:?}", hello),
        Err(e) => println!("Failed to parse ClientHello: {:?}", e),
    }

    Ok(())
}