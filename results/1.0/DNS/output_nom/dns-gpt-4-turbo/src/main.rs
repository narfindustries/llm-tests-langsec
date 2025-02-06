use nom::{
    bytes::complete::{take, take_while},
    number::complete::be_u16,
    sequence::tuple,
    IResult,
};

use std::env;
use std::fs::read;

#[derive(Debug)]
struct DnsHeader {
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
struct DnsQuestion {
    qname: Vec<String>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DnsMessage {
    header: DnsHeader,
    questions: Vec<DnsQuestion>,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }
    let file_content = read(&args[1]).expect("Could not read file");

    match parse_dns_message(&file_content) {
        Ok((_, message)) => println!("{:?}", message),
        Err(e) => println!("Failed to parse DNS message: {:?}", e),
    }
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;

    let qr = (flags >> 15) & 0x1;
    let opcode = (flags >> 11) & 0xf;
    let aa = (flags >> 10) & 0x1;
    let tc = (flags >> 9) & 0x1;
    let rd = (flags >> 8) & 0x1;
    let ra = (flags >> 7) & 0x1;
    let z = (flags >> 4) & 0x7;
    let rcode = flags & 0xf;

    Ok((input, DnsHeader {
        id,
        qr: qr as u8,
        opcode: opcode as u8,
        aa: aa as u8,
        tc: tc as u8,
        rd: rd as u8,
        ra: ra as u8,
        z: z as u8,
        rcode: rcode as u8,
        qdcount,
        ancount,
        nscount,
        arcount,
    }))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DnsMessage> {
    let (mut input, header) = parse_dns_header(input)?;

    let mut questions = Vec::with_capacity(header.qdcount as usize);
    for _ in 0..header.qdcount {
        let (input_next, question) = parse_dns_question(input)?;
        input = input_next;
        questions.push(question);
    }

    Ok((input, DnsMessage {
        header,
        questions,
    }))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_domain_name(input)?;
    let (input, (qtype, qclass)) = tuple((be_u16, be_u16))(input)?;

    Ok((input, DnsQuestion {
        qname,
        qtype,
        qclass,
    }))
}

fn parse_domain_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut input = input;
    let mut parts = Vec::new();

    loop {
        let (input_next, length) = take(1usize)(input)?;
        let length = length[0] as usize;
        if length == 0 {
            break;
        }

        let (input_next, part) = take(length)(input_next)?;
        parts.push(String::from_utf8_lossy(part).to_string());
        input = input_next;
    }

    Ok((input, parts))
}