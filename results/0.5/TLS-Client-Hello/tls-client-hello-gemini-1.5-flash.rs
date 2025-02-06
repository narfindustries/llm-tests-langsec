use nom::{
    bytes::complete::take,
    combinator::{map, all_consuming},
    multi::{length_count, length_value},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::fs;
use std::env;

#[derive(Debug)]
struct ClientHello {
    protocol_version: (u8, u8),
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
    let (rem, (protocol_version, random, session_id, cipher_suites, compression_methods, extensions)) = tuple((
        map(tuple((be_u8, be_u8)), |(a, b)| (a, b)),
        take(32usize),
        length_count(be_u8, take(1usize)),
        length_count(be_u16, be_u16),
        length_count(be_u8, take(1usize)),
        length_count(be_u16, parse_extension),
    ))(input)?;

    Ok((
        rem,
        ClientHello {
            protocol_version,
            random: random.try_into().unwrap(),
            session_id: session_id.0.to_vec(),
            cipher_suites: cipher_suites.iter().map(|x| *x).collect(),
            compression_methods: compression_methods.0.to_vec(),
            extensions,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (rem, (extension_type, extension_data)) = length_value(be_u16, take(be_u16))(input)?;
    Ok((
        rem,
        Extension {
            extension_type,
            extension_data: extension_data.to_vec(),
        },
    ))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Something went wrong reading the file");

    match all_consuming(parse_client_hello)(&contents) {
        Ok((_, client_hello)) => println!("{:#?}", client_hello),
        Err(e) => eprintln!("Error parsing Client Hello: {:?}", e),
    };
}
