use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct IcmpHeader {
    type_: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
    data: Vec<u8>,
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, type_) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = be_u32(input)?;
    let (input, data) = take(input.len())(input)?;

    Ok((
        input,
        IcmpHeader {
            type_,
            code,
            checksum,
            rest_of_header,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_icmp(&data) {
        Ok((_, icmp_header)) => {
            println!("Parsed ICMP Header: {:?}", icmp_header);
        }
        Err(e) => {
            eprintln!("Failed to parse ICMP header: {:?}", e);
        }
    }
}