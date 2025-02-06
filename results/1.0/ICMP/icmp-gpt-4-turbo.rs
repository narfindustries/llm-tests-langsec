use nom::{
    number::complete::{be_u8, be_u16},
    IResult, multi::count,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct IcmpPacket {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: Vec<u8>,  // Specific to type and code
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;

    // Determine the length of the rest of the header based on type and code
    // For simplicity, gathering the rest as raw bytes
    let length = input.len();
    let (input, rest_of_header) = count(be_u8, length)(input)?;

    Ok((
        input,
        IcmpPacket {
            icmp_type,
            code,
            checksum,
            rest_of_header,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    match parse_icmp_packet(&contents) {
        Ok((_remaining, packet)) => {
            println!("Parsed ICMP Packet: {:?}", packet);
        },
        Err(error) => {
            println!("Error parsing packet: {:?}", error);
        },
    }

    Ok(())
}