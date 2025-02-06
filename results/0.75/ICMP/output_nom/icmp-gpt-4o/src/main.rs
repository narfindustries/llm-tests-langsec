/// A Rust program using the `nom` crate to parse ICMP packets according to RFC 792.
/// It reads a binary file provided as a command line argument and parses it using ICMP header structure.

use nom::{
    IResult,
    bytes::complete::take,
    number::complete::{be_u8, be_u16, be_u32},
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::process;

/// ICMP Header structure
#[derive(Debug)]
struct IcmpHeader {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: Vec<u8>,
    data: Vec<u8>,
}

/// Parser for ICMP header using nom
fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    
    // The rest_of_header is 4 bytes for most types.
    let (input, rest_of_header) = take(4usize)(input)?;
    
    // Remaining part of the packet is considered as data.
    let (input, data) = take(input.len())(input)?;

    Ok((
        input,
        IcmpHeader {
            icmp_type,
            code,
            checksum,
            rest_of_header: rest_of_header.to_vec(),
            data: data.to_vec(),
        },
    ))
}

fn main() {
    // Collect command line arguments
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        process::exit(1);
    }

    // Open the binary file
    let mut file = match File::open(&args[1]) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            process::exit(1);
        }
    };

    // Read the file content into a buffer
    let mut buffer = Vec::new();
    if let Err(e) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", e);
        process::exit(1);
    }

    // Parse the ICMP packet
    match parse_icmp(&buffer) {
        Ok((_, icmp_header)) => {
            println!("Parsed ICMP Packet: {:?}", icmp_header);
        }
        Err(e) => {
            eprintln!("Failed to parse ICMP packet: {:?}", e);
            process::exit(1);
        }
    }
}