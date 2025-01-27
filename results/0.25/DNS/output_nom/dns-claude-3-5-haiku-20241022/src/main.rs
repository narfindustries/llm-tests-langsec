use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    branch::alt,
    bytes::streaming::{tag, take, take_while},
    combinator::{map, opt},
    error::ErrorKind,
    multi::{count, many0, many_m_n},
    number::streaming::{be_u8, be_u16, be_u32},
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

fn parse_dns_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut names = Vec::new();
    let mut current_input = input;

    loop {
        let (remaining, length) = be_u8(current_input)?;
        
        if length == 0 {
            break;
        }

        if (length & 0xC0) == 0xC0 {
            // Pointer to another location in the packet
            let (_, offset) = be_u16(current_input)?;
            let pointer_offset = offset & 0x3FFF;
            // TODO: Implement pointer resolution
            break;
        }

        let (next_input, name_bytes) = take(length)(remaining)?;
        let name = String::from_utf8_lossy(name_bytes).into_owned();
        names.push(name);
        current_input = next_input;
    }

    Ok((current_input, names))
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (remaining, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((
        be_u16,
        parse_dns_flags,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
    ))(input)?;

    Ok((
        remaining,
        DNSHeader {
            id,
            flags,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (remaining, (qname, qtype, qclass)) = tuple((
        parse_dns_name,
        be_u16,
        be_u16,
    ))(input)?;

    Ok((
        remaining,
        DNSQuestion {
            qname,
            qtype,
            qclass,
        },
    ))
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    let (remaining, (name, rtype, rclass, ttl, rdlength)) = tuple((
        parse_dns_name,
        be_u16,
        be_u16,
        be_u32,
        be_u16,
    ))(input)?;

    let (final_remaining, rdata) = take(rdlength)(remaining)?;

    Ok((
        final_remaining,
        DNSResourceRecord {
            name,
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DNSPacket> {
    let (remaining, header) = parse_dns_header(input)?;

    let (remaining, questions) = count(parse_dns_question, header.qdcount as usize)(remaining)?;
    let (remaining, answers) = count(parse_dns_resource_record, header.ancount as usize)(remaining)?;
    let (remaining, authorities) = count(parse_dns_resource_record, header.nscount as usize)(remaining)?;
    let (remaining, additionals) = count(parse_dns_resource_record, header.arcount as usize)(remaining)?;

    Ok((
        remaining,
        DNSPacket {
            header,
            questions,
            answers,
            authorities,
            additionals,
        },
    ))
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