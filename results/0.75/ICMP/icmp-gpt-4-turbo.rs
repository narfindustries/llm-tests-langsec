use nom::{
    bytes::complete::take,
    number::complete::{be_u8, be_u16},
    IResult,
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

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (rest_of_header, data) = take(4usize)(input)?;

    Ok((
        data,
        IcmpPacket {
            icmp_type,
            code,
            checksum,
            rest_of_header: rest_of_header.to_vec(),
            data: data.to_vec(),
        },
    ))
}

fn read_file_contents(filename: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ICMP packet file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let file_contents = read_file_contents(filename)?;
    
    match parse_icmp_packet(&file_contents) {
        Ok((remaining, icmp_packet)) => {
            println!("{:?}", icmp_packet);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining unprocessed", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Failed to parse ICMP packet: {:?}", e);
        }
    }

    Ok(())
}