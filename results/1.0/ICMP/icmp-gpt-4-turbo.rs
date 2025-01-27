use nom::{
    IResult, 
    bytes::complete::{take},
    number::complete::{be_u8, be_u16}
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct IcmpPacket {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: [u8; 4],
    data: Vec<u8>,
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = take(4usize)(input)?;
    let (rest, data) = take(input.len())(input)?;
    
    let mut fixed_header = [0u8; 4];
    fixed_header.copy_from_slice(rest_of_header);

    Ok((rest, IcmpPacket {
        icmp_type,
        code,
        checksum,
        rest_of_header: fixed_header,
        data: data.to_vec(),
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_icmp(&buffer) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(error) => println!("Failed to parse ICMP packet: {:?}", error),
    }

    Ok(())
}