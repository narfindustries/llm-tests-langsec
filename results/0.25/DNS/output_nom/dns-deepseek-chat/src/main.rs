use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    fs::File,
    io::Read,
    net::Ipv4Addr,
    str::{from_utf8, Utf8Error},
};

#[derive(Debug)]
struct DNSHeader {
    id: u16,
    flags: u16,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct DNSQuestion {
    qname: String,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DNSRecord {
    name: String,
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
    answers: Vec<DNSRecord>,
    authorities: Vec<DNSRecord>,
    additionals: Vec<DNSRecord>,
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) =
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
    Ok((
        input,
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

fn parse_dns_name(input: &[u8]) -> IResult<&[u8], String> {
    let mut remaining = input;
    let mut name = String::new();
    let mut first = true;

    loop {
        let (new_remaining, len) = be_u8(remaining)?;
        if len == 0 {
            remaining = new_remaining;
            break;
        }
        if !first {
            name.push('.');
        }
        first = false;
        let (new_remaining, label) = take(len)(new_remaining)?;
        name.push_str(from_utf8(label).map_err(|_| {
            nom::Err::Error((input, nom::error::ErrorKind::Tag))
        })?);
        remaining = new_remaining;
    }

    Ok((remaining, name))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (input, qname) = parse_dns_name(input)?;
    let (input, (qtype, qclass)) = tuple((be_u16, be_u16))(input)?;
    Ok((
        input,
        DNSQuestion {
            qname,
            qtype,
            qclass,
        },
    ))
}

fn parse_dns_record(input: &[u8]) -> IResult<&[u8], DNSRecord> {
    let (input, name) = parse_dns_name(input)?;
    let (input, (rtype, rclass, ttl, rdlength)) =
        tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((
        input,
        DNSRecord {
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
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = nom::multi::count(parse_dns_question, header.qdcount as usize)(input)?;
    let (input, answers) = nom::multi::count(parse_dns_record, header.ancount as usize)(input)?;
    let (input, authorities) = nom::multi::count(parse_dns_record, header.nscount as usize)(input)?;
    let (input, additionals) = nom::multi::count(parse_dns_record, header.arcount as usize)(input)?;

    Ok((
        input,
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
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let (_, packet) = parse_dns_packet(&buffer).map_err(|e| {
        eprintln!("Failed to parse DNS packet: {:?}", e);
        std::process::exit(1);
    })?;

    println!("{:#?}", packet);

    Ok(())
}