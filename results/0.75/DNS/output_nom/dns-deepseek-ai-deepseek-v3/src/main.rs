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
    flags: u16,
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
struct DNSResourceRecord {
    name: Vec<String>,
    rtype: u16,
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
    additional: Vec<DNSResourceRecord>,
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

fn parse_dns_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut remaining = input;
    loop {
        let (new_remaining, len) = take(1u8)(remaining)?;
        let len = len[0] as usize;
        if len == 0 {
            break;
        }
        let (new_remaining, label) = take(len)(new_remaining)?;
        labels.push(String::from_utf8_lossy(label).to_string());
        remaining = new_remaining;
    }
    Ok((remaining, labels))
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

fn parse_dns_rr(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    let (input, name) = parse_dns_name(input)?;
    let (input, (rtype, class, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((
        input,
        DNSResourceRecord {
            name,
            rtype,
            class,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DNSMessage> {
    let (input, header) = parse_dns_header(input)?;
    let mut questions = Vec::new();
    let mut answers = Vec::new();
    let mut authorities = Vec::new();
    let mut additional = Vec::new();

    let mut remaining = input;
    for _ in 0..header.qdcount {
        let (new_remaining, question) = parse_dns_question(remaining)?;
        questions.push(question);
        remaining = new_remaining;
    }

    for _ in 0..header.ancount {
        let (new_remaining, rr) = parse_dns_rr(remaining)?;
        answers.push(rr);
        remaining = new_remaining;
    }

    for _ in 0..header.nscount {
        let (new_remaining, rr) = parse_dns_rr(remaining)?;
        authorities.push(rr);
        remaining = new_remaining;
    }

    for _ in 0..header.arcount {
        let (new_remaining, rr) = parse_dns_rr(remaining)?;
        additional.push(rr);
        remaining = new_remaining;
    }

    Ok((
        remaining,
        DNSMessage {
            header,
            questions,
            answers,
            authorities,
            additional,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dns_message(&buffer) {
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Error parsing DNS message: {:?}", e),
    }
}