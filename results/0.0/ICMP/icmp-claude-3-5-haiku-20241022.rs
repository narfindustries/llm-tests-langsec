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
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ICMPPacket {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: Option<Vec<u8>>,
    payload: Option<Vec<u8>>,
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], ICMPPacket> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;

    let (input, rest_of_header) = match icmp_type {
        0 | 8 | 13 | 14 | 15 | 16 => opt(take(4usize))(input)?,
        3 | 4 | 5 | 11 | 12 => opt(take(4usize))(input)?,
        _ => opt(take(4usize))(input)?,
    };

    let (input, payload) = opt(take(input.len()))(input)?;

    Ok((input, ICMPPacket {
        icmp_type,
        code,
        checksum,
        rest_of_header: rest_of_header.map(|v| v.to_vec()),
        payload: payload.map(|v| v.to_vec()),
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_icmp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed ICMP Packet: {:?}", packet);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}