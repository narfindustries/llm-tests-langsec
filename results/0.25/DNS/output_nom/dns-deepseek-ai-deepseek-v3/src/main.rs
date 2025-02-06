use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::length_data,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::{
    fs::File,
    io::Read,
    net::{Ipv4Addr, Ipv6Addr},
};

#[derive(Debug)]
struct DNSHeader {
    id: u16,
    qr: u8,
    opcode: u8,
    aa: u8,
    tc: u8,
    rd: u8,
    ra: u8,
    z: u8,
    rcode: u8,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct DNSQuestion {
    qname: Vec<String>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
enum DNSRecordData {
    A(Ipv4Addr),
    AAAA(Ipv6Addr),
    NS(Vec<String>),
    CNAME(Vec<String>),
    MX { preference: u16, exchange: Vec<String> },
    TXT(Vec<u8>),
    SOA {
        mname: Vec<String>,
        rname: Vec<String>,
        serial: u32,
        refresh: u32,
        retry: u32,
        expire: u32,
        minimum: u32,
    },
    PTR(Vec<String>),
    Other(Vec<u8>),
}

#[derive(Debug)]
struct DNSRecord {
    name: Vec<String>,
    rtype: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: DNSRecordData,
}

#[derive(Debug)]
struct DNSMessage {
    header: DNSHeader,
    questions: Vec<DNSQuestion>,
    answers: Vec<DNSRecord>,
    authorities: Vec<DNSRecord>,
    additionals: Vec<DNSRecord>,
}

fn parse_label(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = nom::number::complete::u8(input)?;
    let (input, label) = take(len)(input)?;
    Ok((input, String::from_utf8_lossy(label).to_string()))
}

fn parse_domain_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let (input, labels) = nom::multi::many0(parse_label)(input)?;
    let (input, _) = tag(&[0])(input)?;
    Ok((input, labels))
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) =
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
    let qr = (flags >> 15) as u8;
    let opcode = ((flags >> 11) & 0xF) as u8;
    let aa = ((flags >> 10) & 1) as u8;
    let tc = ((flags >> 9) & 1) as u8;
    let rd = ((flags >> 8) & 1) as u8;
    let ra = ((flags >> 7) & 1) as u8;
    let z = ((flags >> 4) & 0x7) as u8;
    let rcode = (flags & 0xF) as u8;
    Ok((
        input,
        DNSHeader {
            id,
            qr,
            opcode,
            aa,
            tc,
            rd,
            ra,
            z,
            rcode,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (input, qname) = parse_domain_name(input)?;
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
    let (input, name) = parse_domain_name(input)?;
    let (input, (rtype, class, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    let rdata = match rtype {
        1 => DNSRecordData::A(Ipv4Addr::new(rdata[0], rdata[1], rdata[2], rdata[3])),
        28 => DNSRecordData::AAAA(Ipv6Addr::from([
            rdata[0], rdata[1], rdata[2], rdata[3], rdata[4], rdata[5], rdata[6], rdata[7],
            rdata[8], rdata[9], rdata[10], rdata[11], rdata[12], rdata[13], rdata[14], rdata[15],
        ])),
        2 => DNSRecordData::NS(parse_domain_name(rdata)?.1),
        5 => DNSRecordData::CNAME(parse_domain_name(rdata)?.1),
        15 => {
            let (_, preference) = be_u16(rdata)?;
            let (_, exchange) = parse_domain_name(&rdata[2..])?;
            DNSRecordData::MX {
                preference,
                exchange,
            }
        }
        16 => DNSRecordData::TXT(rdata.to_vec()),
        6 => {
            let (_, (mname, rname, serial, refresh, retry, expire, minimum)) = tuple((
                parse_domain_name,
                parse_domain_name,
                be_u32,
                be_u32,
                be_u32,
                be_u32,
                be_u32,
            ))(rdata)?;
            DNSRecordData::SOA {
                mname,
                rname,
                serial,
                refresh,
                retry,
                expire,
                minimum,
            }
        }
        12 => DNSRecordData::PTR(parse_domain_name(rdata)?.1),
        _ => DNSRecordData::Other(rdata.to_vec()),
    };
    Ok((
        input,
        DNSRecord {
            name,
            rtype,
            class,
            ttl,
            rdlength,
            rdata,
        },
    ))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DNSMessage> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = nom::multi::count(parse_dns_question, header.qdcount as usize)(input)?;
    let (input, answers) = nom::multi::count(parse_dns_record, header.ancount as usize)(input)?;
    let (input, authorities) = nom::multi::count(parse_dns_record, header.nscount as usize)(input)?;
    let (input, additionals) = nom::multi::count(parse_dns_record, header.arcount as usize)(input)?;
    Ok((
        input,
        DNSMessage {
            header,
            questions,
            answers,
            authorities,
            additionals,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        std::process::exit(1);
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");
    match parse_dns_message(&buffer) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse DNS message: {:?}", e),
    }
}