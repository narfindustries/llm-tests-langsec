use nom::{
    combinator::{map},
    error::{Error, ErrorKind},
    multi::{count},
    number::streaming::{be_u8, be_u16, be_u32},
    sequence::{tuple},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct DnsHeader {
    transaction_id: u16,
    flags: DnsFlags,
    question_count: u16,
    answer_count: u16,
    authority_count: u16,
    additional_count: u16,
}

#[derive(Debug)]
struct DnsFlags {
    qr: bool,
    opcode: u8,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    rcode: u8,
}

#[derive(Debug)]
struct DnsQuestion {
    name: Vec<String>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DnsResourceRecord {
    name: Vec<String>,
    record_type: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DnsMessage {
    header: DnsHeader,
    questions: Vec<DnsQuestion>,
    answers: Vec<DnsResourceRecord>,
    authorities: Vec<DnsResourceRecord>,
    additionals: Vec<DnsResourceRecord>,
}

fn parse_dns_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut current_input = input;

    loop {
        let (remaining, length) = be_u8(current_input)?;
        
        if length == 0 {
            return Ok((remaining, labels));
        }

        if length & 0xC0 == 0xC0 {
            return Err(nom::Err::Error(Error::new(input, ErrorKind::Tag)));
        }

        let (next_input, label) = nom::bytes::streaming::take(length)(remaining)?;
        labels.push(String::from_utf8_lossy(label).into_owned());
        current_input = next_input;
    }
}

fn parse_dns_flags(input: &[u8]) -> IResult<&[u8], DnsFlags> {
    map(be_u16, |raw_flags| DnsFlags {
        qr: (raw_flags & 0x8000) != 0,
        opcode: ((raw_flags & 0x7800) >> 11) as u8,
        aa: (raw_flags & 0x0400) != 0,
        tc: (raw_flags & 0x0200) != 0,
        rd: (raw_flags & 0x0100) != 0,
        ra: (raw_flags & 0x0080) != 0,
        rcode: (raw_flags & 0x000F) as u8,
    })(input)
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    map(
        tuple((
            be_u16,
            parse_dns_flags,
            be_u16,
            be_u16,
            be_u16,
            be_u16
        )),
        |(transaction_id, flags, question_count, answer_count, authority_count, additional_count)| 
        DnsHeader {
            transaction_id,
            flags,
            question_count,
            answer_count,
            authority_count,
            additional_count,
        }
    )(input)
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    map(
        tuple((
            parse_dns_name,
            be_u16,
            be_u16
        )),
        |(name, qtype, qclass)| DnsQuestion {
            name,
            qtype,
            qclass,
        }
    )(input)
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    map(
        tuple((
            parse_dns_name,
            be_u16,
            be_u16,
            be_u32,
            be_u16,
            take_variable_length
        )),
        |(name, record_type, class, ttl, rdlength, rdata)| DnsResourceRecord {
            name,
            record_type,
            class,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        }
    )(input)
}

fn take_variable_length(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let (remaining, length) = be_u16(input)?;
    nom::bytes::streaming::take(length)(remaining)
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DnsMessage> {
    map(
        tuple((
            parse_dns_header,
            count(parse_dns_question, 1),
            count(parse_dns_resource_record, 1),
            count(parse_dns_resource_record, 1),
            count(parse_dns_resource_record, 1)
        )),
        |(header, questions, answers, authorities, additionals)| DnsMessage {
            header,
            questions,
            answers,
            authorities,
            additionals,
        }
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_packet_file>", args[0]);
        std::process::exit(1);
    }

    let file_contents = fs::read(&args[1])?;
    
    match parse_dns_message(&file_contents) {
        Ok((_, dns_message)) => {
            println!("Parsed DNS Message: {:?}", dns_message);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            Err("Failed to parse DNS message".into())
        }
    }
}