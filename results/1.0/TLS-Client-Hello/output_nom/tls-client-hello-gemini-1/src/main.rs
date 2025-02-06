use nom::{
    bytes::complete::take,
    combinator::opt,
    multi::length_count,
    number::complete::{be_u16, be_u8},
    IResult, Err, error::ErrorKind
};
use std::env;
use std::fs;

#[derive(Debug)]
struct ClientHello {
    client_version: u16,
    random: Vec<u8>,
    legacy_session_id: Option<Vec<u8>>,
    cipher_suites: Vec<u16>,
    compression_methods: Vec<u8>,
    extensions: Vec<u8>,
}

fn client_hello(input: &[u8]) -> IResult<&[u8], ClientHello, ErrorKind> {
    let (rem, client_version) = be_u16(input)?;
    let (rem, random) = take(32usize)(rem)?;
    let (rem, legacy_session_id_len) = opt(be_u8)(rem)?;
    let legacy_session_id = legacy_session_id_len.and_then(|len| {
        let len = len as usize;
        match take(len)(rem) {
            Ok((rem1, legacy_session_id)) => Some((rem1, legacy_session_id.to_vec())),
            Err(_) => None,
        }
    });
    let (rem, legacy_session_id) = match legacy_session_id {
        Some((rem, data)) => (rem, Some(data)),
        None => (rem, None),
    };

    let (rem, cipher_suites_len) = be_u16(rem)?;
    let (rem, cipher_suites) = length_count(be_u16, be_u16)(rem)?;

    let (rem, compression_methods_len) = be_u8(rem)?;
    let (rem, compression_methods) = length_count(be_u8, be_u8)(rem)?;

    let (rem, extensions_len) = be_u16(rem)?;
    let (rem, extensions) = take(extensions_len as usize)(rem)?;

    Ok((
        rem,
        ClientHello {
            client_version,
            random: random.to_vec(),
            legacy_session_id,
            cipher_suites: cipher_suites.to_vec(),
            compression_methods: compression_methods.to_vec(),
            extensions: extensions.to_vec(),
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
    let contents = fs::read(filename).expect("Something went wrong reading the file");
    match client_hello(&contents) {
        Ok((rem, client_hello)) => {
            println!("Parsed Client Hello:");
            println!("  Client Version: 0x{:04x}", client_hello.client_version);
            println!("  Random: {:?}", client_hello.random);
            println!("  Legacy Session ID: {:?}", client_hello.legacy_session_id);
            println!("  Cipher Suites: {:?}", client_hello.cipher_suites);
            println!(
                "  Compression Methods: {:?}",
                client_hello.compression_methods
            );
            println!("  Extensions: {:?}", client_hello.extensions);
            println!("Remaining bytes: {:?}", rem);
        }
        Err(e) => {
            println!("Error parsing Client Hello: {:?}", e);
        }
    }
}
