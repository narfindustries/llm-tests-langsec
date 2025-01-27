use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct IPHeader {
    version_ihl: u8,
    tos: u8,
    len: u16,
    id: u16,
    flags_offset: u16,
    ttl: u8,
    protocol: u8,
    checksum: u16,
    src: [u8; 4],
    dst: [u8; 4],
}

#[derive(Debug)]
struct ICMPHeader {
    type_: u8,
    code: u8,
    checksum: u16,
    rest: Vec<u8>,
}


fn parse_ip_header(input: &[u8]) -> IResult<&[u8], IPHeader> {
    let (input, version_ihl) = take(1usize)(input)?;
    let (input, tos) = take(1usize)(input)?;
    let (input, len) = be_u16(input)?;
    let (input, id) = be_u16(input)?;
    let (input, flags_offset) = be_u16(input)?;
    let (input, ttl) = take(1usize)(input)?;
    let (input, protocol) = take(1usize)(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, src) = take(4usize)(input)?;
    let (input, dst) = take(4usize)(input)?;

    Ok((
        input,
        IPHeader {
            version_ihl: version_ihl[0],
            tos: tos[0],
            len: len,
            id: id,
            flags_offset: flags_offset,
            ttl: ttl[0],
            protocol: protocol[0],
            checksum: checksum,
            src: src.try_into().unwrap(),
            dst: dst.try_into().unwrap(),
        },
    ))
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], ICMPHeader> {
    let (input, type_) = take(1usize)(input)?;
    let (input, code) = take(1usize)(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest) = take(input.len())(input)?;
    Ok((
        input,
        ICMPHeader {
            type_: type_[0],
            code: code[0],
            checksum: checksum,
            rest: rest.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_ip_header(&buffer) {
        Ok((remaining, ip_header)) => {
            println!("IP Header: {:?}", ip_header);
            if ip_header.protocol == 1 { //ICMP protocol
                match parse_icmp_header(remaining) {
                    Ok((_, icmp_header)) => {
                        println!("ICMP Header: {:?}", icmp_header);
                    }
                    Err(e) => println!("Error parsing ICMP header: {:?}", e),
                }
            }
        }
        Err(e) => println!("Error parsing IP header: {:?}", e),
    }
}
