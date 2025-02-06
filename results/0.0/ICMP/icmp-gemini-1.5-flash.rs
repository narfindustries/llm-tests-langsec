use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::be_u16,
    IResult,
};
use std::fs;
use std::env;

#[derive(Debug)]
struct IcmpHeader {
    type_: u8,
    code: u8,
    checksum: u16,
    identifier: u16,
    sequence_number: u16,
    data: Vec<u8>,
}

fn icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, type_) = take(1usize)(input)?;
    let (input, code) = take(1usize)(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, identifier) = be_u16(input)?;
    let (input, sequence_number) = be_u16(input)?;
    let (input, data) = take(input.len())(input)?;

    Ok((
        input,
        IcmpHeader {
            type_: type_[0],
            code: code[0],
            checksum,
            identifier,
            sequence_number,
            data: data.to_vec(),
        },
    ))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <icmp_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Something went wrong reading the file");

    match icmp_header(&contents) {
        Ok((_, header)) => {
            println!("ICMP Header: {:?}", header);
        }
        Err(e) => {
            eprintln!("Error parsing ICMP header: {:?}", e);
        }
    }
}

