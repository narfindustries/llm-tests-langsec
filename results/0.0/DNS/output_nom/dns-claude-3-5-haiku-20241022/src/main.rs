use nom::{
    bytes::streaming::{take as take_bytes},
    combinator::map,
    multi::count,
    number::streaming::{be_u16, be_u32, be_u8},
    sequence::tuple,
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
    Unknown(u16),
}

#[derive(Debug)]
enum DNSClass {
    IN,
    Other(u16),
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
    rcode: u8,
}

#[derive(Debug)]
struct DNSQuestion {
    name: Vec<String>,
    record_type: DNSRecordType,
    class: DNSClass,
}

#[derive(Debug)]
struct DNSResourceRecord {
    name: Vec<String>,
    record_type: DNSRecordType,
    class: DNSClass,
    ttl: u32,
    data: Vec<u8>,
}

#[derive(Debug)]
struct DNSMessage {
    header: DNSHeader,
    questions: Vec<DNSQuestion>,
    answers: Vec<DNSResourceRecord>,
    authorities: Vec<DNSResourceRecord>,
    additionals: Vec<DNSResourceRecord>,
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
            let (_, _pointer) = be_u8(remaining)?;
            return Ok((remaining, labels));
        }

        let (next_input, label) = take_bytes(length as usize)(remaining)?;
        labels.push(String::from_utf8_lossy(label).to_string());
        current_input = next_input;
    }
}

fn parse_dns_record_type(input: &[u8]) -> IResult<&[u8], DNSRecordType> {
    map(be_u16, |type_num| match type_num {
        1 => DNSRecordType::A,
        2 => DNSRecordType::NS,
        5 => DNSRecordType::CNAME,
        6 => DNSRecordType::SOA,
        12 => DNSRecordType::PTR,
        15 => DNSRecordType::MX,
        28 => DNSRecordType::AAAA,
        x => DNSRecordType::Unknown(x),
    })(input)
}

fn parse_dns_class(input: &[u8]) -> IResult<&[u8], DNSClass> {
    map(be_u16, |class_num| match class_num {
        1 => DNSClass::IN,
        x => DNSClass::Other(x),
    })(input)
}

fn parse_dns_flags(input: &[u8]) -> IResult<&[u8], DNSFlags> {
    map(be_u16, |flags| DNSFlags {
        qr: (flags & 0x8000) != 0,
        opcode: ((flags & 0x7800) >> 11) as u8,
        aa: (flags & 0x0400) != 0,
        tc: (flags & 0x0200) != 0,
        rd: (flags & 0x0100) != 0,
        ra: (flags & 0x0080) != 0,
        rcode: (flags & 0x000F) as u8,
    })(input)
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    map(
        tuple((
            be_u16,
            parse_dns_flags,
            be_u16,
            be_u16,
            be_u16,
            be_u16,
        )),
        |(transaction_id, flags, question_count, answer_count, authority_count, additional_count)| DNSHeader {
            transaction_id,
            flags,
            question_count,
            answer_count,
            authority_count,
            additional_count,
        },
    )(input)
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    map(
        tuple((
            parse_dns_name,
            parse_dns_record_type,
            parse_dns_class,
        )),
        |(name, record_type, class)| DNSQuestion {
            name,
            record_type,
            class,
        },
    )(input)
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    map(
        tuple((
            parse_dns_name,
            parse_dns_record_type,
            parse_dns_class,
            be_u32,
            be_u16,
            |i| take_bytes(be_u16(i)?.1 as usize)(i)
        )),
        |(name, record_type, class, ttl, data_length, data)| DNSResourceRecord {
            name,
            record_type,
            class,
            ttl,
            data: data.to_vec(),
        },
    )(input)
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DNSMessage> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = count(parse_dns_question, header.question_count as usize)(input)?;
    let (input, answers) = count(parse_dns_resource_record, header.answer_count as usize)(input)?;
    let (input, authorities) = count(parse_dns_resource_record, header.authority_count as usize)(input)?;
    let (input, additionals) = count(parse_dns_resource_record, header.additional_count as usize)(input)?;

    Ok((input, DNSMessage {
        header,
        questions,
        answers,
        authorities,
        additionals,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_packet_file>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    let dns_data = fs::read(file_path)?;

    match parse_dns_message(&dns_data) {
        Ok((_, dns_message)) => {
            println!("Parsed DNS Message: {:?}", dns_message);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse DNS message: {:?}", e);
            std::process::exit(1);
        }
    }
}