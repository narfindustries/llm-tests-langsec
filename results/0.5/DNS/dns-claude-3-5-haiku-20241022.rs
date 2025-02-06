use nom::{
    bits::{streaming::take as take_bits},
    bytes::streaming::{take},
    combinator::{map},
    multi::{count},
    number::streaming::{be_u16, be_u32, be_u8},
    sequence::{tuple},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
enum DNSRecordType {
    A,
    NS,
    CNAME,
    SOA,
    PTR,
    MX,
    AAAA,
    AXFR,
    ANY,
}

#[derive(Debug)]
enum DNSClass {
    IN,
    CH,
    HS,
    ANY,
}

#[derive(Debug)]
struct DNSHeader {
    transaction_id: u16,
    flags: DNSFlags,
    question_count: u16,
    answer_count: u16,
    authority_count: u16,
    additional_count: u16,
}

#[derive(Debug)]
struct DNSFlags {
    qr: bool,
    opcode: u8,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: u8,
    rcode: u8,
}

#[derive(Debug)]
struct DNSQuestion {
    qname: Vec<String>,
    qtype: DNSRecordType,
    qclass: DNSClass,
}

#[derive(Debug)]
struct DNSResourceRecord {
    name: Vec<String>,
    record_type: DNSRecordType,
    class: DNSClass,
    ttl: i32,
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

fn parse_dns_flags(input: &[u8]) -> IResult<&[u8], DNSFlags> {
    map(be_u16, |flags| {
        DNSFlags {
            qr: (flags & 0x8000) != 0,
            opcode: ((flags & 0x7800) >> 11) as u8,
            aa: (flags & 0x0400) != 0,
            tc: (flags & 0x0200) != 0,
            rd: (flags & 0x0100) != 0,
            ra: (flags & 0x0080) != 0,
            z: ((flags & 0x0070) >> 4) as u8,
            rcode: (flags & 0x000F) as u8,
        }
    })(input)
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, (transaction_id, flags, question_count, answer_count, authority_count, additional_count)) =
        tuple((
            be_u16,
            parse_dns_flags,
            be_u16,
            be_u16,
            be_u16,
            be_u16,
        ))(input)?;

    Ok((
        input,
        DNSHeader {
            transaction_id,
            flags,
            question_count,
            answer_count,
            authority_count,
            additional_count,
        },
    ))
}

fn parse_dns_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut current_input = input;

    loop {
        let (next_input, length) = be_u8(current_input)?;
        
        if length == 0 {
            current_input = next_input;
            break;
        }

        if length & 0xC0 == 0xC0 {
            let (_, offset) = take_bits::<_, u16, _, _>(10)(current_input)?;
            let compressed_offset = offset as usize;
            let compressed_input = &input[compressed_offset..];
            let (_, compressed_label) = parse_dns_name(compressed_input)?;
            labels.extend(compressed_label);
            current_input = next_input;
            break;
        }

        let (next_input, label) = take(length)(current_input)?;
        labels.push(String::from_utf8_lossy(label).into_owned());
        current_input = next_input;
    }

    Ok((current_input, labels))
}

fn parse_dns_record_type(input: &[u8]) -> IResult<&[u8], DNSRecordType> {
    map(be_u16, |type_code| {
        match type_code {
            1 => DNSRecordType::A,
            2 => DNSRecordType::NS,
            5 => DNSRecordType::CNAME,
            6 => DNSRecordType::SOA,
            12 => DNSRecordType::PTR,
            15 => DNSRecordType::MX,
            28 => DNSRecordType::AAAA,
            252 => DNSRecordType::AXFR,
            255 => DNSRecordType::ANY,
            _ => panic!("Unknown record type"),
        }
    })(input)
}

fn parse_dns_class(input: &[u8]) -> IResult<&[u8], DNSClass> {
    map(be_u16, |class_code| {
        match class_code {
            1 => DNSClass::IN,
            3 => DNSClass::CH,
            4 => DNSClass::HS,
            255 => DNSClass::ANY,
            _ => panic!("Unknown class"),
        }
    })(input)
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (input, qname) = parse_dns_name(input)?;
    let (input, qtype) = parse_dns_record_type(input)?;
    let (input, qclass) = parse_dns_class(input)?;

    Ok((input, DNSQuestion { qname, qtype, qclass }))
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    let (input, name) = parse_dns_name(input)?;
    let (input, record_type) = parse_dns_record_type(input)?;
    let (input, class) = parse_dns_class(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength)(input)?;

    Ok((
        input,
        DNSResourceRecord {
            name,
            record_type,
            class,
            ttl: ttl as i32,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DNSMessage> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = count(parse_dns_question, header.question_count as usize)(input)?;
    let (input, answers) = count(parse_dns_resource_record, header.answer_count as usize)(input)?;
    let (input, authorities) = count(parse_dns_resource_record, header.authority_count as usize)(input)?;
    let (input, additionals) = count(parse_dns_resource_record, header.additional_count as usize)(input)?;

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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_packet_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    let dns_packet = fs::read(file_path)?;

    match parse_dns_message(&dns_packet) {
        Ok((_, dns_message)) => {
            println!("Parsed DNS Message: {:?}", dns_message);
            Ok(())
        }
        Err(err) => {
            eprintln!("Failed to parse DNS packet: {:?}", err);
            std::process::exit(1);
        }
    }
}