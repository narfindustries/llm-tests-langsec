use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct IPHeader {
    version_ihl: u8,
    diffserv: u8,
    len: u16,
    id: u16,
    flags_offset: u16,
    ttl: u8,
    protocol: u8,
    checksum: u16,
    source: [u8; 4],
    destination: [u8; 4],
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
    let (rest, diffserv) = take(1usize)(rest)?;
    let (rest, len) = be_u16(rest)?;
    let (rest, id) = be_u16(rest)?;
    let (rest, flags_offset) = be_u16(rest)?;
    let (rest, ttl) = take(1usize)(rest)?;
    let (rest, protocol) = take(1usize)(rest)?;
    let (rest, checksum) = be_u16(rest)?;
    let (rest, source) = take(4usize)(rest)?;
    let (rest, destination) = take(4usize)(rest)?;

    Ok((
        rest,
        IPHeader {
            version_ihl,
            diffserv,
            len,
            id,
            flags_offset,
            ttl,
            protocol,
            checksum,
            source: source.try_into().unwrap(),
            destination: destination.try_into().unwrap(),
        },
    ))
}


fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], ICMPHeader> {
    let (rest, type_) = take(1usize)(input)?;
    let (rest, code) = take(1usize)(rest)?;
    let (rest, checksum) = be_u16(rest)?;
    let rest = &rest[..];
    Ok((rest, ICMPHeader { type_: type_[0], code: code[0], checksum, rest: rest.to_vec() }))
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], (IPHeader, ICMPHeader)> {
    let (rest, ip_header) = parse_ip_header(input)?;
    let (rest, icmp_header) = parse_icmp_header(rest)?;
    Ok((rest, (ip_header, icmp_header)))
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

    match parse_icmp_packet(&buffer) {
        Ok((_, (ip_header, icmp_header))) => {
            println!("IP Header: {:?}", ip_header);
            println!("ICMP Header: {:?}", icmp_header);
        }
        Err(e) => {
            println!("Error parsing packet: {:?}", e);
        }
    }
}
