use nom::{
    bytes::complete::{tag, take, take_until},
    combinator::{map, map_res, opt, value},
    multi::length_count,
    number::complete::{be_u16, be_u24, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct ClientHello {
    client_version: (u8, u8),
    random: [u8; 32],
    session_id: Option<Vec<u8>>,
    cipher_suites: Vec<(u8, u8)>,
    compression_methods: Vec<u8>,
    extensions: Option<Vec<u8>>,
}

fn parse_client_hello(input: &[u8]) -> IResult<&[u8], ClientHello> {
    let (input, client_version) = tuple((be_u8, be_u8))(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, session_id) = opt(preceded(be_u8, take_until(b"\x00")))(input)?;
    let session_id = session_id.map(|s| s[..s.len()-1].to_vec());

    let (input, cipher_suites) = length_count(be_u16, tuple((be_u8, be_u8)))(input)?;
    let (input, compression_methods) = length_count(be_u8, be_u8)(input)?;
    let (input, extensions) = opt(preceded(be_u16, take_until(b"\x00")))(input)?;
    let extensions = extensions.map(|s| s[..s.len()-1].to_vec());

    Ok((
        input,
        ClientHello {
            client_version,
            random: random.try_into().unwrap(),
            session_id,
            cipher_suites,
            compression_methods,
            extensions,
        },
    ))
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_client_hello(&buffer) {
        Ok((_, client_hello)) => {
            println!("{:#?}", client_hello);
        }
        Err(e) => {
            println!("Error parsing Client Hello: {:?}", e);
        }
    }

    Ok(())
}
