use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    bytes::streaming::{tag, take},
    error::ErrorKind,
    multi::many0,
    number::streaming::{be_u8, be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ICMPHeader {
    icmp_type: u8,
    icmp_code: u8,
    checksum: u16,
    rest_of_header: Option<Vec<u8>>,
    payload: Option<Vec<u8>>,
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], ICMPHeader> {
    let (input, (icmp_type, icmp_code, checksum)) = tuple((be_u8, be_u8, be_u16))(input)?;

    let (input, rest_of_header) = match icmp_type {
        0 | 8 => {
            let (input, identifier) = be_u16(input)?;
            let (input, sequence_number) = be_u16(input)?;
            (input, Some(vec![
                (identifier >> 8) as u8,
                (identifier & 0xFF) as u8,
                (sequence_number >> 8) as u8,
                (sequence_number & 0xFF) as u8,
            ]))
        }
        3 | 11 => {
            let (input, unused) = take(4usize)(input)?;
            (input, Some(unused.to_vec()))
        }
        _ => (input, None),
    };

    let (input, payload) = many0(be_u8)(input)?;

    Ok((input, ICMPHeader {
        icmp_type,
        icmp_code,
        checksum,
        rest_of_header,
        payload: if !payload.is_empty() { Some(payload) } else { None },
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

    match parse_icmp_header(&buffer) {
        Ok((_, icmp)) => {
            println!("Parsed ICMP Header: {:?}", icmp);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}