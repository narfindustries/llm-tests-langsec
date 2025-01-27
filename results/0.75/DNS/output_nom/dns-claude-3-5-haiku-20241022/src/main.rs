use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{be_u16, be_u8, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct DNSPacket {
    header: DNSHeader,
    questions: Vec<DNSQuestion>,
    answers: Vec<DNSResourceRecord>,
    authorities: Vec<DNSResourceRecord>,
    additionals: Vec<DNSResourceRecord>,
}

#[derive(Debug)]
struct DNSHeader {
    id: u16,
    flags: DNSFlags,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct DNSFlags {
    qr: bool,
    opcode: u8,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: u8,
    rcode: u8,
}

#[derive(Debug)]
struct DNSQuestion {
    qname: Vec<String>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DNSResourceRecord {
    name: Vec<String>,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

fn parse_dns_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut current_input = input;

    loop {
        let (remaining, length) = be_u8(current_input)?;
        
        if length == 0 {
            return Ok((remaining, labels));
        }

        if length & 0xC0 == 0xC0 {
            let (_, pointer) = be_u16(current_input)?;
            let offset = pointer & 0x3FFF;
            return Ok((remaining, labels));
        }

        let (remaining, label) = take(length)(current_input)?;
        labels.push(String::from_utf8_lossy(label).to_string());
        current_input = remaining;
    }
}

fn parse_dns_flags(input: &[u8]) -> IResult<&[u8], DNSFlags> {
    map(be_u16, |flags| DNSFlags {
        qr: (flags & 0x8000) != 0,
        opcode: ((flags & 0x7800) >> 11) as u8,
        aa: (flags & 0x0400) != 0,
        tc: (flags & 0x0200) != 0,
        rd: (flags & 0x0100) != 0,
        ra: (flags & 0x0080) != 0,
        z: ((flags & 0x0070) >> 4) as u8,
        rcode: (flags & 0x000F) as u8,
    })(input)
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((
        be_u16,
        parse_dns_flags,
        be_u16,
        be_u16,
        be_u16,
        be_u16
    ))(input)?;

    Ok((input, DNSHeader {
        id,
        flags,
        qdcount,
        ancount,
        nscount,
        arcount,
    }))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (input, (qname, qtype, qclass)) = tuple((
        parse_dns_name,
        be_u16,
        be_u16
    ))(input)?;

    Ok((input, DNSQuestion {
        qname,
        qtype,
        qclass,
    }))
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    let (input, (name, rtype, rclass, ttl, rdlength)) = tuple((
        parse_dns_name,
        be_u16,
        be_u16,
        be_u32,
        be_u16
    ))(input)?;

    let (input, rdata) = take(rdlength)(input)?;

    Ok((input, DNSResourceRecord {
        name,
        rtype,
        rclass,
        ttl,
        rdlength,
        rdata: rdata.to_vec(),
    }))
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DNSPacket> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = count(parse_dns_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_dns_resource_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_dns_resource_record, header.nscount as usize)(input)?;
    let (input, additionals) = count(parse_dns_resource_record, header.arcount as usize)(input)?;

    Ok((input, DNSPacket {
        header,
        questions,
        answers,
        authorities,
        additionals,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_packet_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let packet_data = fs::read(filename).expect("Failed to read file");

    match parse_dns_packet(&packet_data) {
        Ok((_, packet)) => {
            println!("Parsed DNS Packet: {:?}", packet);
        }
        Err(e) => {
            eprintln!("Failed to parse DNS packet: {:?}", e);
        }
    }
}