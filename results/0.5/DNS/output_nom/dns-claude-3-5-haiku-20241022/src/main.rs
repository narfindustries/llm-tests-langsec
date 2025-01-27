use nom::{
    bytes::complete::{take, take_while},
    combinator::{map, opt},
    multi::{many0, many_m_n},
    number::complete::{be_u16, be_u8},
    sequence::{tuple, pair},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    let mut remaining = input;

    loop {
        let (rest, length) = be_u8(remaining)?;
        if length == 0 {
            return Ok((rest, labels));
        }

        if length & 0xC0 == 0xC0 {
            let (rest, offset) = map(be_u8, |second| ((length & 0x3F) << 8 | second) as usize)(rest)?;
            // Handle pointer to previous label
            return Ok((rest, labels));
        }

        let (rest, label) = take(length)(remaining)?;
        labels.push(String::from_utf8_lossy(label).to_string());
        remaining = rest;
    }
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((
        be_u16,
        parse_dns_flags,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
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

fn parse_dns_flags(input: &[u8]) -> IResult<&[u8], DNSFlags> {
    let (input, first_byte) = be_u8(input)?;
    let (input, second_byte) = be_u8(input)?;

    Ok((input, DNSFlags {
        qr: (first_byte & 0x80) != 0,
        opcode: (first_byte >> 3) & 0x0F,
        aa: (first_byte & 0x04) != 0,
        tc: (first_byte & 0x02) != 0,
        rd: (first_byte & 0x01) != 0,
        ra: (second_byte & 0x80) != 0,
        z: (second_byte >> 4) & 0x07,
        rcode: second_byte & 0x0F,
    }))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (input, (qname, qtype, qclass)) = tuple((
        parse_dns_name,
        be_u16,
        be_u16,
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
        map(be_u16, |high| high as u32),
        be_u16,
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
    let (input, questions) = many_m_n(header.qdcount as usize, header.qdcount as usize, parse_dns_question)(input)?;
    let (input, answers) = many_m_n(header.ancount as usize, header.ancount as usize, parse_dns_resource_record)(input)?;
    let (input, authorities) = many_m_n(header.nscount as usize, header.nscount as usize, parse_dns_resource_record)(input)?;
    let (input, additionals) = many_m_n(header.arcount as usize, header.arcount as usize, parse_dns_resource_record)(input)?;

    Ok((input, DNSPacket {
        header,
        questions,
        answers,
        authorities,
        additionals,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_packet_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dns_packet(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed DNS Packet: {:?}", packet);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse DNS packet: {:?}", e);
            Err(e.into())
        }
    }
}