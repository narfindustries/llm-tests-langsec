use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct IcmpHeader {
    type_field: u8,
    code: u8,
    checksum: u16,
    rest: Vec<u8>,
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, (type_field, code, checksum)) = tuple((be_u8, be_u8, be_u16))(input)?;
    let (input, rest) = take(input.len())(input)?;
    Ok((
        input,
        IcmpHeader {
            type_field,
            code,
            checksum,
            rest: rest.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_icmp_header(&data) {
        Ok((_, icmp_header)) => {
            println!("Parsed ICMP Header: {:?}", icmp_header);
        }
        Err(e) => {
            eprintln!("Failed to parse ICMP header: {:?}", e);
        }
    }
}