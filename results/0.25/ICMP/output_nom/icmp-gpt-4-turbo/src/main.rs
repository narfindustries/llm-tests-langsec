use nom::{
    IResult,
    bytes::complete::{take},
    number::complete::{be_u8, be_u16},
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct IcmpPacket {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: Vec<u8>,
    data: Vec<u8>,
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = take(4usize)(input)?;
    let (input, data) = take(input.len())(input)?;

    Ok((input, IcmpPacket {
        icmp_type,
        code,
        checksum,
        rest_of_header: rest_of_header.to_vec(),
        data: data.to_vec(),
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_icmp(&buffer) {
        Ok((_, icmp_packet)) => {
            println!("{:?}", icmp_packet);
        },
        Err(e) => {
            println!("Failed to parse ICMP packet: {:?}", e);
        }
    }

    Ok(())
}