use nom::{
    bytes::complete::{tag, take},
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
    qname: String,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DNSResourceRecord {
    name: String,
    r#type: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DNSMessage {
    header: DNSHeader,
    questions: Vec<DNSQuestion>,
    answers: Vec<DNSResourceRecord>,
    authorities: Vec<DNSResourceRecord>,
    additionals: Vec<DNSResourceRecord>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
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

fn parse_name(input: &[u8]) -> IResult<&[u8], String> {
    let mut remaining = input;
    let mut name = String::new();
    loop {
        let (input, len) = take(1u8)(remaining)?;
        let len = len[0] as usize;
        if len == 0 {
            remaining = input;
            break;
        }
        if len & 0xC0 == 0xC0 {
            let (input, offset) = take(1u8)(input)?;
            let offset = ((len & 0x3F) as u16) << 8 | offset[0] as u16;
            let (_, compressed_name) = parse_name(&input[offset as usize..])?;
            name.push_str(&compressed_name);
            remaining = input;
            break;
        } else {
            let (input, label) = take(len)(input)?;
            name.push_str(&String::from_utf8_lossy(label));
            name.push('.');
            remaining = input;
        }
    }
    Ok((remaining, name))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (input, qname) = parse_name(input)?;
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

fn parse_rr(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    let (input, name) = parse_name(input)?;
    let (input, (r#type, class, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((
        input,
        DNSResourceRecord {
            name,
            r#type,
            class,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DNSMessage> {
    let (input, header) = parse_header(input)?;
    let mut questions = Vec::new();
    let mut answers = Vec::new();
    let mut authorities = Vec::new();
    let mut additionals = Vec::new();
    let mut remaining = input;
    for _ in 0..header.qdcount {
        let (input, question) = parse_question(remaining)?;
        questions.push(question);
        remaining = input;
    }
    for _ in 0..header.ancount {
        let (input, answer) = parse_rr(remaining)?;
        answers.push(answer);
        remaining = input;
    }
    for _ in 0..header.nscount {
        let (input, authority) = parse_rr(remaining)?;
        authorities.push(authority);
        remaining = input;
    }
    for _ in 0..header.arcount {
        let (input, additional) = parse_rr(remaining)?;
        additionals.push(additional);
        remaining = input;
    }
    Ok((
        remaining,
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
        return;
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");
    match parse_dns_message(&buffer) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse DNS message: {:?}", e),
    }
}