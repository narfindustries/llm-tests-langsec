use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    error::ErrorKind,
    multi::many_m_n,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

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

fn parse_label(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = be_u8(input)?;
    let (input, label) = map_res(take(len), std::str::from_utf8)(input)?;
    Ok((input, label.to_string()))
}

fn parse_qname(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut input = input;
    loop {
        let (new_input, label) = parse_label(input)?;
        if label.is_empty() {
            break;
        }
        labels.push(label);
        input = new_input;
    }
    Ok((input, labels))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) =
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
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
    let (input, qname) = parse_qname(input)?;
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

fn parse_record(input: &[u8]) -> IResult<&[u8], DnsRecord> {
    let (input, name) = parse_qname(input)?;
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
    let (input, header) = parse_header(input)?;
    let (input, questions) = many_m_n(header.qdcount as usize, header.qdcount as usize, parse_question)(input)?;
    let (input, answers) = many_m_n(header.ancount as usize, header.ancount as usize, parse_record)(input)?;
    let (input, authorities) = many_m_n(header.nscount as usize, header.nscount as usize, parse_record)(input)?;
    let (input, additionals) = many_m_n(header.arcount as usize, header.arcount as usize, parse_record)(input)?;
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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_binary_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dns_message(&buffer) {
        Ok((_, dns_message)) => println!("{:#?}", dns_message),
        Err(e) => eprintln!("Failed to parse DNS message: {:?}", e),
    }

    Ok(())
}