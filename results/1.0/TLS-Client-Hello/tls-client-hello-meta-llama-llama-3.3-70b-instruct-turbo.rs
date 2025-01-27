use nom::{
    bytes::complete::{take, take_till, take_while},
    combinator::map,
    error::{context, ErrorKind, ParseError},
    multi::length_data,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug, PartialEq)]
struct Random {
    gmt_unix_time: u32,
    random_bytes: Vec<u8>,
}

fn parse_random(input: &[u8]) -> IResult<&[u8], Random> {
    context(
        "Random",
        tuple((
            be_u32,
            take(28usize),
        )),
    )
    .map(|(gmt_unix_time, random_bytes)| Random {
        gmt_unix_time,
        random_bytes: random_bytes.to_vec(),
    })
    .parse(input)
}

#[derive(Debug, PartialEq)]
struct SessionId {
    session_id: Vec<u8>,
}

fn parse_session_id(input: &[u8]) -> IResult<&[u8], SessionId> {
    context(
        "SessionId",
        map(
            length_data(be_u8),
            |session_id| SessionId { session_id },
        ),
    )(input)
}

#[derive(Debug, PartialEq)]
struct CipherSuite {
    cipher_suite: u16,
}

fn parse_cipher_suite(input: &[u8]) -> IResult<&[u8], CipherSuite> {
    context(
        "CipherSuite",
        map(be_u16, |cipher_suite| CipherSuite { cipher_suite }),
    )(input)
}

#[derive(Debug, PartialEq)]
struct CompressionMethod {
    compression_method: u8,
}

fn parse_compression_method(input: &[u8]) -> IResult<&[u8], CompressionMethod> {
    context(
        "CompressionMethod",
        map(be_u8, |compression_method| CompressionMethod {
            compression_method,
        }),
    )(input)
}

#[derive(Debug, PartialEq)]
struct Extension {
    extension_type: u16,
    extension_data: Vec<u8>,
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    context(
        "Extension",
        tuple((
            be_u16,
            length_data(be_u16),
        )),
    )
    .map(|(extension_type, extension_data)| Extension {
        extension_type,
        extension_data,
    })
    .parse(input)
}

#[derive(Debug, PartialEq)]
struct ClientHello {
    record_layer_version: u16,
    random: Random,
    session_id: SessionId,
    cipher_suites: Vec<CipherSuite>,
    compression_methods: Vec<CompressionMethod>,
    extensions: Vec<Extension>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    context(
        "ClientHello",
        tuple((
            be_u16,
            parse_random,
            parse_session_id,
            map(
                length_data(be_u16),
                |cipher_suites| {
                    cipher_suites
                        .chunks_exact(std::mem::size_of::<u16>())
                        .map(|cipher_suite| CipherSuite {
                            cipher_suite: u16::from_be_bytes([
                                cipher_suite[0],
                                cipher_suite[1],
                            ]),
                        })
                        .collect()
                },
            ),
            map(
                length_data(be_u8),
                |compression_methods| {
                    compression_methods
                        .iter()
                        .map(|&compression_method| CompressionMethod {
                            compression_method,
                        })
                        .collect()
                },
            ),
            map(
                length_data(be_u16),
                |extensions| {
                    let mut extensions_list = Vec::new();
                    let mut remaining = extensions;
                    while !remaining.is_empty() {
                        match parse_extension(remaining) {
                            Ok((next, extension)) => {
                                extensions_list.push(extension);
                                remaining = next;
                            }
                            Err(_) => break,
                        }
                    }
                    extensions_list
                },
            ),
        )),
    )
    .map(
        |(
            record_layer_version,
            random,
            session_id,
            cipher_suites,
            compression_methods,
            extensions,
        )| ClientHello {
            record_layer_version,
            random,
            session_id,
            cipher_suites,
            compression_methods,
            extensions,
        },
    )
    .parse(input)
}

fn main() -> std::io::Result<()> {
    let file_path = std::env::args().nth(1).unwrap();
    let file = File::open(file_path)?;
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input)?;
    match parse_client_hello(&input) {
        Ok((_, client_hello)) => println!("{:?}", client_hello),
        Err(err) => println!("{:?}", err),
    }
    Ok(())
}