use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::many0,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::net::Ipv4Addr;
use std::str;

#[derive(Debug)]
struct DnsHeader {
    id: u16,
    flags: u16,
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
struct DnsRecord {
    name: Vec<String>,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DnsPacket {
    header: DnsHeader,
    questions: Vec<DnsQuestion>,
    answers: Vec<DnsRecord>,
    authorities: Vec<DnsRecord>,
    additionals: Vec<DnsRecord>,
}

fn parse_labels(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut i = input;
    loop {
        let (rest, length) = be_u8(i)?;
        if length == 0 {
            return Ok((rest, labels));
        }
        let (rest, label) = take(length)(rest)?;
        labels.push(str::from_utf8(label).unwrap().to_string());
        i = rest;
    }
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    map(
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16)),
        |(id, flags, qdcount, ancount, nscount, arcount)| DnsHeader {
            id,
            flags,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    )(input)
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    map(
        tuple((parse_labels, be_u16, be_u16)),
        |(qname, qtype, qclass)| DnsQuestion { qname, qtype, qclass },
    )(input)
}

fn parse_record(input: &[u8]) -> IResult<&[u8], DnsRecord> {
    let (input, name) = parse_labels(input)?;
    let (input, (rtype, rclass, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((
        input,
        DnsRecord {
            name,
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DnsPacket> {
    let (input, header) = parse_header(input)?;
    let (input, questions) = many0(parse_question)(input)?;
    let (input, answers) = many0(parse_record)(input)?;
    let (input, authorities) = many0(parse_record)(input)?;
    let (input, additionals) = many0(parse_record)(input)?;
    Ok((
        input,
        DnsPacket {
            header,
            questions,
            answers,
            authorities,
            additionals,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_packet_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dns_packet(&buffer) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Failed to parse DNS packet: {:?}", e),
    }
}