use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    branch::alt,
    bytes::streaming::{tag, take},
    combinator::{map, opt},
    error::ErrorKind,
    multi::{count, many0},
    number::streaming::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct DnsHeader {
    id: u16,
    qr: bool,
    opcode: u8,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: u8,
    rcode: u8,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct DnsQuestion {
    qname: Vec<String>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DnsResourceRecord {
    name: Vec<String>,
    record_type: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DnsMessage {
    header: DnsHeader,
    questions: Vec<DnsQuestion>,
    answers: Vec<DnsResourceRecord>,
    authorities: Vec<DnsResourceRecord>,
    additionals: Vec<DnsResourceRecord>,
}

fn parse_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut current_input = input;

    loop {
        let (remaining, length) = be_u8(current_input)?;
        
        if length == 0 {
            return Ok((remaining, labels));
        }

        if length & 0xC0 == 0xC0 {
            // Compression pointer
            let (_, offset) = be_u16(current_input)?;
            let compression_offset = offset & 0x3FFF;
            break Ok((remaining, labels));
        }

        let (next_input, label) = take(length)(remaining)?;
        labels.push(String::from_utf8_lossy(label).to_string());
        current_input = next_input;
    }
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, id) = be_u16(input)?;
    let (input, flags) = be_u16(input)?;
    let (input, qdcount) = be_u16(input)?;
    let (input, ancount) = be_u16(input)?;
    let (input, nscount) = be_u16(input)?;
    let (input, arcount) = be_u16(input)?;

    Ok((input, DnsHeader {
        id,
        qr: (flags & 0x8000) != 0,
        opcode: ((flags >> 11) & 0x0F) as u8,
        aa: (flags & 0x0400) != 0,
        tc: (flags & 0x0200) != 0,
        rd: (flags & 0x0100) != 0,
        ra: (flags & 0x0080) != 0,
        z: ((flags >> 4) & 0x07) as u8,
        rcode: (flags & 0x0F) as u8,
        qdcount,
        ancount,
        nscount,
        arcount,
    }))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_name(input)?;
    let (input, qtype) = be_u16(input)?;
    let (input, qclass) = be_u16(input)?;

    Ok((input, DnsQuestion {
        qname,
        qtype,
        qclass,
    }))
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    let (input, name) = parse_name(input)?;
    let (input, record_type) = be_u16(input)?;
    let (input, class) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength)(input)?;

    Ok((input, DnsResourceRecord {
        name,
        record_type,
        class,
        ttl,
        rdlength,
        rdata: rdata.to_vec(),
    }))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DnsMessage> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = count(parse_dns_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_resource_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_resource_record, header.nscount as usize)(input)?;
    let (input, additionals) = count(parse_resource_record, header.arcount as usize)(input)?;

    Ok((input, DnsMessage {
        header,
        questions,
        answers,
        authorities,
        additionals,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <dns_packet_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let dns_packet = fs::read(filename)?;

    match parse_dns_message(&dns_packet) {
        Ok((_, dns_message)) => {
            println!("Parsed DNS Message: {:#?}", dns_message);
            Ok(())
        },
        Err(e) => {
            eprintln!("Failed to parse DNS message: {:?}", e);
            std::process::exit(1);
        }
    }
}