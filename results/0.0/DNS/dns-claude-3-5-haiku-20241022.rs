use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{be_u16, be_u8},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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

#[derive(Debug)]
struct DNSPacket {
    header: DNSHeader,
    questions: Vec<DNSQuestion>,
    answers: Vec<DNSResourceRecord>,
    authorities: Vec<DNSResourceRecord>,
    additionals: Vec<DNSResourceRecord>,
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
    map(
        tuple((
            be_u16,
            parse_dns_flags,
            be_u16,
            be_u16,
            be_u16,
            be_u16,
        )),
        |(id, flags, qdcount, ancount, nscount, arcount)| DNSHeader {
            id,
            flags,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    )(input)
}

fn parse_dns_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut names = Vec::new();
    let mut current_input = input;

    loop {
        let (remaining, length) = be_u8(current_input)?;
        if length == 0 {
            break;
        }

        let (next_input, name) = take(length)(remaining)?;
        names.push(String::from_utf8_lossy(name).to_string());
        current_input = next_input;
    }

    Ok((current_input, names))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    map(
        tuple((parse_dns_name, be_u16, be_u16)),
        |(qname, qtype, qclass)| DNSQuestion {
            qname,
            qtype,
            qclass,
        },
    )(input)
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    map(
        tuple((
            parse_dns_name,
            be_u16,
            be_u16,
            be_u32,
            be_u16,
            take_rdata,
        )),
        |(name, rtype, rclass, ttl, rdlength, rdata)| DNSResourceRecord {
            name,
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    )(input)
}

fn take_rdata(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (remaining, length) = be_u16(input)?;
    take(length)(remaining)
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DNSPacket> {
    map(
        tuple((
            parse_dns_header,
            count(parse_dns_question, |header: &DNSHeader| header.qdcount as usize),
            count(parse_dns_resource_record, |header: &DNSHeader| header.ancount as usize),
            count(parse_dns_resource_record, |header: &DNSHeader| header.nscount as usize),
            count(parse_dns_resource_record, |header: &DNSHeader| header.arcount as usize),
        )),
        |(header, questions, answers, authorities, additionals)| DNSPacket {
            header,
            questions,
            answers,
            authorities,
            additionals,
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_packet_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_dns_packet(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed DNS Packet: {:?}", packet);
        }
        Err(e) => {
            eprintln!("Error parsing DNS packet: {:?}", e);
        }
    }
}