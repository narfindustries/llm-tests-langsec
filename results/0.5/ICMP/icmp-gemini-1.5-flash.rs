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
    total_length: u16,
    identification: u16,
    flags_fragment_offset: u16,
    ttl: u8,
    protocol: u8,
    header_checksum: u16,
    source_ip: [u8; 4],
    destination_ip: [u8; 4],
}

#[derive(Debug)]
struct ICMPHeader {
    type_: u8,
    code: u8,
    checksum: u16,
    rest: Vec<u8>,
}

fn parse_ip_header(input: &[u8]) -> IResult<&[u8], IPHeader> {
    let (rest, version_ihl) = take(1usize)(input)?;
    let (rest, tos) = take(1usize)(rest)?;
    let (rest, total_length) = be_u16(rest)?;
    let (rest, identification) = be_u16(rest)?;
    let (rest, flags_fragment_offset) = be_u16(rest)?;
    let (rest, ttl) = take(1usize)(rest)?;
    let (rest, protocol) = take(1usize)(rest)?;
    let (rest, header_checksum) = be_u16(rest)?;
    let (rest, source_ip) = take(4usize)(rest)?;
    let (rest, destination_ip) = take(4usize)(rest)?;

    Ok((
        rest,
        IPHeader {
            version_ihl,
            tos,
            total_length,
            identification,
            flags_fragment_offset,
            ttl,
            protocol,
            header_checksum,
            source_ip: source_ip.try_into().unwrap(),
            destination_ip: destination_ip.try_into().unwrap(),
        },
    ))
}


fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], ICMPHeader> {
    let (rest, type_) = take(1usize)(input)?;
    let (rest, code) = take(1usize)(rest)?;
    let (rest, checksum) = be_u16(rest)?;
    let rest = rest;
    Ok((
        rest,
        ICMPHeader {
            type_: type_[0],
            code: code[0],
            checksum,
            rest: rest.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_ip_header(&buffer) {
        Ok((rest, ip_header)) => {
            println!("IP Header: {:?}", ip_header);
            if ip_header.protocol == 1 {
                match parse_icmp_header(rest) {
                    Ok((_, icmp_header)) => {
                        println!("ICMP Header: {:?}", icmp_header);
                    }
                    Err(e) => {
                        eprintln!("Error parsing ICMP header: {:?}", e);
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("Error parsing IP header: {:?}", e);
        }
    }
}
