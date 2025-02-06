use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::take,
    number::complete::{be_u8, be_u16},
};

#[derive(Debug)]
struct IcmpPacket {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
    data: Vec<u8>,
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = be_u32(input)?;
    let (input, data) = take(input.len())(input)?;

    Ok((input, IcmpPacket {
        icmp_type,
        code,
        checksum,
        rest_of_header,
        data: data.to_vec(),
    }))
}

fn be_u32(input: &[u8]) -> IResult<&[u8], u32> {
    let (input, bytes) = take(4usize)(input)?;
    let value = ((bytes[0] as u32) << 24)
              | ((bytes[1] as u32) << 16)
              | ((bytes[2] as u32) << 8)
              | (bytes[3] as u32);
    Ok((input, value))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_icmp(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse ICMP packet: {:?}", e),
    }
}