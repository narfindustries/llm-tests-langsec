use nom::{
    bytes::complete::take,
    combinator::map,
    number::complete::be_u16,
    IResult,
};
use std::fs;
use std::net::{IpAddr, Ipv4Addr};
use std::path::Path;

#[derive(Debug)]
struct IPHeader {
    version: u8,
    ihl: u8,
    diffserv: u8,
    total_length: u16,
    identification: u16,
    flags: u16,
    fragment_offset: u16,
    ttl: u8,
    protocol: u8,
    header_checksum: u16,
    source_ip: IpAddr,
    destination_ip: IpAddr,
}

#[derive(Debug)]
struct ICMPMessage {
    type_: u8,
    code: u8,
    checksum: u16,
    data: Vec<u8>,
}

fn parse_ipv4_addr(input: &[u8]) -> IResult<&[u8], IpAddr> {
    let (rest, addr) = take(4usize)(input)?;
    let ipv4 = Ipv4Addr::new(addr[0], addr[1], addr[2], addr[3]);
    Ok((rest, IpAddr::V4(ipv4)))
}


fn parse_ip_header(input: &[u8]) -> IResult<&[u8], IPHeader> {
    let (rest, version_ihl) = take(1usize)(input)?;
    let version = (version_ihl[0] >> 4) & 0x0F;
    let ihl = version_ihl[0] & 0x0F;
    let (rest, diffserv) = take(1usize)(rest)?;
    let (rest, total_length) = be_u16(rest)?;
    let (rest, identification) = be_u16(rest)?;
    let (rest, flags_offset) = be_u16(rest)?;
    let flags = (flags_offset >> 13) & 0x07;
    let fragment_offset = flags_offset & 0x1FFF;
    let (rest, ttl) = take(1usize)(rest)?;
    let (rest, protocol) = take(1usize)(rest)?;
    let (rest, header_checksum) = be_u16(rest)?;
    let (rest, source_ip) = parse_ipv4_addr(rest)?;
    let (rest, destination_ip) = parse_ipv4_addr(rest)?;

    Ok((
        rest,
        IPHeader {
            version,
            ihl,
            diffserv: diffserv[0],
            total_length,
            identification,
            flags,
            fragment_offset,
            ttl: ttl[0],
            protocol: protocol[0],
            header_checksum,
            source_ip,
            destination_ip,
        },
    ))
}


fn parse_icmp_message(input: &[u8]) -> IResult<&[u8], ICMPMessage> {
    let (rest, type_) = map(take(1u8), |v: &[u8]| v[0])(input)?;
    let (rest, code) = map(take(1u8), |v: &[u8]| v[0])(rest)?;
    let (rest, checksum) = be_u16(rest)?;
    let data_len = input.len() - 3;
    let (rest, data) = take(data_len)(rest)?;
    Ok((
        rest,
        ICMPMessage {
            type_,
            code,
            checksum,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = Path::new(&args[1]);
    let file_content = match fs::read(file_path) {
        Ok(content) => content,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            std::process::exit(1);
        }
    };

    match parse_icmp_message(&file_content) {
        Ok((_, icmp_message)) => {
            println!("Parsed ICMP message: {:?}", icmp_message);
        }
        Err(e) => {
            eprintln!("Error parsing ICMP message: {:?}", e);
            std::process::exit(1);
        }
    }
}
