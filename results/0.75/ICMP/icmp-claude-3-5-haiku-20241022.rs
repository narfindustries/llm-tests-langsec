use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    bytes::streaming::{tag, take},
    combinator::{map, opt},
    error::ErrorKind,
    multi::count,
    number::streaming::{be_u8, be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;
use std::process;

#[derive(Debug)]
struct IcmpHeader {
    icmp_type: u8,
    icmp_code: u8,
    checksum: u16,
    rest_of_header: Option<Vec<u8>>,
    payload: Option<Vec<u8>>,
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, icmp_code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;

    let (input, rest_of_header) = match icmp_type {
        0 | 8 => opt(take(4usize))(input)?, // Echo Request/Reply
        3 | 11 => opt(take(4usize))(input)?, // Destination Unreachable, Time Exceeded
        5 => opt(take(4usize))(input)?, // Redirect
        _ => opt(take(4usize))(input)?,
    };

    let (input, payload) = opt(take(input.len()))(input)?;

    Ok((
        input,
        IcmpHeader {
            icmp_type,
            icmp_code,
            checksum,
            rest_of_header: rest_of_header.map(|v| v.to_vec()),
            payload: payload.map(|v| v.to_vec()),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <icmp_binary_file>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let data = fs::read(filename).expect("Unable to read file");

    match parse_icmp(&data) {
        Ok((_, parsed_icmp)) => {
            println!("Parsed ICMP: {:?}", parsed_icmp);
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            process::exit(1);
        }
    }
}