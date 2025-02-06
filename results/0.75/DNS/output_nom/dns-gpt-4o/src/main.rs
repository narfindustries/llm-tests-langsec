use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

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
struct DnsResourceRecord {
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
    answers: Vec<DnsResourceRecord>,
    authorities: Vec<DnsResourceRecord>,
    additionals: Vec<DnsResourceRecord>,
}

fn parse_domain_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut remainder = input;
    loop {
        let (rest, length) = be_u8(remainder)?;
        if length == 0 {
            return Ok((rest, labels));
        }
        let (rest, label) = map_res(take(length), std::str::from_utf8)(rest)?;
        labels.push(label.to_string());
        remainder = rest;
    }
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
    Ok((
        input,
        DnsHeader {
            id,
            flags,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_domain_name(input)?;
    let (input, qtype) = be_u16(input)?;
    let (input, qclass) = be_u16(input)?;
    Ok((
        input,
        DnsQuestion {
            qname,
            qtype,
            qclass,
        },
    ))
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    let (input, name) = parse_domain_name(input)?;
    let (input, rtype) = be_u16(input)?;
    let (input, rclass) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((
        input,
        DnsResourceRecord {
            name,
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_packet(input: &[u8]) -> IResult<&[u8], DnsPacket> {
    let (input, header) = parse_header(input)?;
    let (input, questions) = nom::multi::count(parse_question, header.qdcount as usize)(input)?;
    let (input, answers) = nom::multi::count(parse_resource_record, header.ancount as usize)(input)?;
    let (input, authorities) = nom::multi::count(parse_resource_record, header.nscount as usize)(input)?;
    let (input, additionals) = nom::multi::count(parse_resource_record, header.arcount as usize)(input)?;
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
        eprintln!("Usage: {} <input file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = fs::read(filename).expect("Failed to read file");

    match parse_packet(&data) {
        Ok((_remaining, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Failed to parse DNS packet: {:?}", e),
    }
}