use std::fs::File;
use std::env;
use std::io::Read;
use nom::{
    IResult,
    number::complete::{be_u8, be_u16, be_u32},
    bytes::complete::take,
    combinator::{map, map_res},
    multi::{count, length_data},
    sequence::{tuple, preceded},
};

// DNS Header
#[derive(Debug)]
struct DnsHeader {
    id: u16,
    flags: u16,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
    Ok((input, DnsHeader { id, flags, qdcount, ancount, nscount, arcount }))
}

// DNS Question
#[derive(Debug)]
struct DnsQuestion {
    qname: Vec<String>,
    qtype: u16,
    qclass: u16,
}

fn parse_label(input: &[u8]) -> IResult<&[u8], String> {
    map_res(length_data(be_u8), |s: &[u8]| std::str::from_utf8(s).map(String::from))(input)
}

fn parse_qname(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut input = input;
    loop {
        let (i, label) = parse_label(input)?;
        if label.is_empty() {
            return Ok((i, labels));
        }
        labels.push(label);
        input = i;
    }
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_qname(input)?;
    let (input, qtype) = be_u16(input)?;
    let (input, qclass) = be_u16(input)?;
    Ok((input, DnsQuestion { qname, qtype, qclass }))
}

// DNS Resource Record
#[derive(Debug)]
struct DnsResourceRecord {
    rname: Vec<String>,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    let (input, rname) = parse_qname(input)?;
    let (input, rtype) = be_u16(input)?;
    let (input, rclass) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((input, DnsResourceRecord {
        rname,
        rtype,
        rclass,
        ttl,
        rdlength,
        rdata: rdata.to_vec(),
    }))
}

// Complete DNS Message
#[derive(Debug)]
struct DnsMessage {
    header: DnsHeader,
    questions: Vec<DnsQuestion>,
    answers: Vec<DnsResourceRecord>,
    authority: Vec<DnsResourceRecord>,
    additional: Vec<DnsResourceRecord>,
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DnsMessage> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = count(parse_dns_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_dns_resource_record, header.ancount as usize)(input)?;
    let (input, authority) = count(parse_dns_resource_record, header.nscount as usize)(input)?;
    let (input, additional) = count(parse_dns_resource_record, header.arcount as usize)(input)?;

    Ok((input, DnsMessage {
        header,
        questions,
        answers,
        authority,
        additional,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_dns_message(&buffer) {
        Ok((_, dns_message)) => println!("{:?}", dns_message),
        Err(e) => eprintln!("Error parsing DNS message: {:?}", e),
    }
}