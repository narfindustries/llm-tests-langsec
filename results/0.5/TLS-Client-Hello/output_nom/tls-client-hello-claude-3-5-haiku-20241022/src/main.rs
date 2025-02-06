use nom::{
    bytes::complete::{take, tag},
    multi::{count, many0},
    number::complete::{be_u8, be_u16, be_u24, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TlsClientHello {
    handshake_type: u8,
    length: u32,
    client_version: u16,
    random: [u8; 32],
    legacy_session_id_length: u8,
    legacy_session_id: Vec<u8>,
    cipher_suites_length: u16,
    cipher_suites: Vec<u16>,
    compression_methods_length: u8,
    compression_methods: Vec<u8>,
    extensions_length: u16,
    extensions: Vec<Extension>,
}

#[derive(Debug)]
struct Extension {
    extension_type: u16,
    extension_length: u16,
    extension_data: Vec<u8>,
}

fn parse_tls_client_hello(input: &[u8]) -> IResult<&[u8], TlsClientHello> {
    let (input, handshake_type) = be_u8(input)?;
    let (input, length) = be_u24(input)?;
    let (input, client_version) = be_u16(input)?;
    let (input, random) = take(32usize)(input)?;
    let (input, legacy_session_id_length) = be_u8(input)?;
    let (input, legacy_session_id) = take(legacy_session_id_length as usize)(input)?;
    let (input, cipher_suites_length) = be_u16(input)?;
    let (input, cipher_suites) = count(be_u16, cipher_suites_length as usize / 2)(input)?;
    let (input, compression_methods_length) = be_u8(input)?;
    let (input, compression_methods) = take(compression_methods_length as usize)(input)?;
    let (input, extensions_length) = be_u16(input)?;
    let (input, extensions) = parse_extensions(extensions_length)(input)?;

    Ok((input, TlsClientHello {
        handshake_type,
        length: length as u32,
        client_version,
        random: random.try_into().unwrap(),
        legacy_session_id_length,
        legacy_session_id: legacy_session_id.to_vec(),
        cipher_suites_length,
        cipher_suites,
        compression_methods_length,
        compression_methods: compression_methods.to_vec(),
        extensions_length,
        extensions,
    }))
}

fn parse_extensions(total_length: u16) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<Extension>> {
    move |input: &[u8]| {
        let mut extensions = Vec::new();
        let mut remaining_input = input;
        let mut parsed_length = 0;

        while parsed_length < total_length as usize {
            let (next_input, extension_type) = be_u16(remaining_input)?;
            let (next_input, extension_length) = be_u16(next_input)?;
            let (next_input, extension_data) = take(extension_length as usize)(next_input)?;

            extensions.push(Extension {
                extension_type,
                extension_length,
                extension_data: extension_data.to_vec(),
            });

            remaining_input = next_input;
            parsed_length += 4 + extension_length as usize;
        }

        Ok((remaining_input, extensions))
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tls_client_hello(&buffer) {
        Ok((_, client_hello)) => {
            println!("Parsed TLS Client Hello: {:?}", client_hello);
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}