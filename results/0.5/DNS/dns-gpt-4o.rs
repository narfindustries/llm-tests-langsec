use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
struct DnsMessage {
    header: DnsHeader,
    questions: Vec<DnsQuestion>,
    answers: Vec<DnsRecord>,
    authorities: Vec<DnsRecord>,
    additionals: Vec<DnsRecord>,
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, id) = be_u16(input)?;
    let (input, flags) = be_u16(input)?;
    let (input, qdcount) = be_u16(input)?;
    let (input, ancount) = be_u16(input)?;
    let (input, nscount) = be_u16(input)?;
    let (input, arcount) = be_u16(input)?;
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

fn parse_labels(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut input = input;
    loop {
        let (i, length) = be_u8(input)?;
        if length == 0 {
            break;
        }
        let (i, label) = take(length)(i)?;
        labels.push(String::from_utf8_lossy(label).into_owned());
        input = i;
    }
    Ok((input, labels))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_labels(input)?;
    let (input, qtype) = be_u16(input)?;
    let (input, qclass) = be_u16(input)?;
    Ok((input, DnsQuestion { qname, qtype, qclass }))
}

fn parse_record(input: &[u8]) -> IResult<&[u8], DnsRecord> {
    let (input, name) = parse_labels(input)?;
    let (input, rtype) = be_u16(input)?;
    let (input, rclass) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
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

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DnsMessage> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = count(parse_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_record, header.nscount as usize)(input)?;
    let (input, additionals) = count(parse_record, header.arcount as usize)(input)?;
    Ok((
        input,
        DnsMessage {
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
        eprintln!("Usage: {} <dns_message_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dns_message(&buffer) {
        Ok((_, message)) => println!("{:?}", message),
        Err(e) => eprintln!("Failed to parse DNS message: {:?}", e),
    }
}