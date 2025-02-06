use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    bytes::streaming::{tag, take},
    error::Error,
    multi::{count, many0},
    sequence::{preceded, tuple},
    IResult,
    number::streaming::{be_u8, be_u16, be_u32},
    combinator::{map, opt},
};
use std::env;
use std::fs;

#[derive(Debug, Clone)]
enum OpCode {
    StandardQuery,
    InverseQuery,
    ServerStatusRequest,
    Reserved,
}

#[derive(Debug, Clone)]
enum ResponseCode {
    NoError,
    FormatError,
    ServerFailure,
    NameError,
    NotImplemented,
    Refused,
    Reserved,
}

#[derive(Debug)]
struct DnsHeader {
    id: u16,
    qr: bool,
    opcode: OpCode,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: u8,
    rcode: ResponseCode,
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
struct ResourceRecord {
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
    answers: Vec<ResourceRecord>,
    authorities: Vec<ResourceRecord>,
    additional: Vec<ResourceRecord>,
}

fn parse_opcode(bits: u8) -> OpCode {
    match bits {
        0 => OpCode::StandardQuery,
        1 => OpCode::InverseQuery,
        2 => OpCode::ServerStatusRequest,
        _ => OpCode::Reserved,
    }
}

fn parse_response_code(bits: u8) -> ResponseCode {
    match bits {
        0 => ResponseCode::NoError,
        1 => ResponseCode::FormatError,
        2 => ResponseCode::ServerFailure,
        3 => ResponseCode::NameError,
        4 => ResponseCode::NotImplemented,
        5 => ResponseCode::Refused,
        _ => ResponseCode::Reserved,
    }
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, id) = be_u16(input)?;
    let (input, first_byte) = be_u8(input)?;
    let (input, second_byte) = be_u8(input)?;

    let qr = (first_byte & 0x80) != 0;
    let opcode = parse_opcode((first_byte & 0x78) >> 3);
    let aa = (first_byte & 0x04) != 0;
    let tc = (first_byte & 0x02) != 0;
    let rd = (first_byte & 0x01) != 0;

    let ra = (second_byte & 0x80) != 0;
    let z = (second_byte & 0x70) >> 4;
    let rcode = parse_response_code(second_byte & 0x0F);

    let (input, qdcount) = be_u16(input)?;
    let (input, ancount) = be_u16(input)?;
    let (input, nscount) = be_u16(input)?;
    let (input, arcount) = be_u16(input)?;

    Ok((input, DnsHeader {
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
    }))
}

fn parse_dns_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut current_input = input;

    loop {
        let (input, length) = be_u8(current_input)?;
        
        if length == 0 {
            return Ok((input, labels));
        }

        if length & 0xC0 == 0xC0 {
            // Compression pointer
            let (input, second_byte) = be_u8(input)?;
            let offset = ((length & 0x3F) << 8) | second_byte;
            // Note: Actual pointer resolution not implemented here
            return Ok((input, labels));
        }

        let (input, label) = take(length)(current_input)?;
        labels.push(String::from_utf8_lossy(label).to_string());
        current_input = input;
    }
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_dns_name(input)?;
    let (input, qtype) = be_u16(input)?;
    let (input, qclass) = be_u16(input)?;

    Ok((input, DnsQuestion {
        qname,
        qtype,
        qclass,
    }))
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], ResourceRecord> {
    let (input, name) = parse_dns_name(input)?;
    let (input, record_type) = be_u16(input)?;
    let (input, class) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength as usize)(input)?;

    Ok((input, ResourceRecord {
        name,
        record_type,
        class,
        ttl,
        rdlength,
        rdata: rdata.to_vec(),
    }))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DnsMessage> {
    let (input, header) = parse_dns_header(input)?;
    
    let (input, questions) = count(parse_dns_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_resource_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_resource_record, header.nscount as usize)(input)?;
    let (input, additional) = count(parse_resource_record, header.arcount as usize)(input)?;

    Ok((input, DnsMessage {
        header,
        questions,
        answers,
        authorities,
        additional,
    }))
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
        Ok((_, parsed_message)) => {
            println!("Parsed DNS Message: {:?}", parsed_message);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing DNS message: {:?}", e);
            Err("DNS parsing failed".into())
        }
    }
}