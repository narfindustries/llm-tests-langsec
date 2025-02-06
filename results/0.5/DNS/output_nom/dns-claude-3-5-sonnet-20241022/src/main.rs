use nom::{
    bytes::complete::{take as byte_take},
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    qname: String,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
enum RData {
    A(Vec<u8>),
    NS(String),
    CNAME(String),
    SOA {
        mname: String,
        rname: String,
        serial: u32,
        refresh: u32,
        retry: u32,
        expire: u32,
        minimum: u32,
    },
    PTR(String),
    MX {
        preference: u16,
        exchange: String,
    },
    TXT(Vec<String>),
    AAAA(Vec<u8>),
    Unknown(Vec<u8>),
}

#[derive(Debug)]
struct ResourceRecord {
    name: String,
    type_: u16,
    class: u16,
    ttl: u32,
    rdata: RData,
}

#[derive(Debug)]
struct DnsMessage {
    header: DnsHeader,
    questions: Vec<DnsQuestion>,
    answers: Vec<ResourceRecord>,
    authorities: Vec<ResourceRecord>,
    additionals: Vec<ResourceRecord>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, id) = be_u16(input)?;
    let (input, flags) = be_u16(input)?;
    let (input, (qdcount, ancount, nscount, arcount)) = tuple((be_u16, be_u16, be_u16, be_u16))(input)?;

    Ok((
        input,
        DnsHeader {
            id,
            qr: ((flags >> 15) & 0x1) as u8,
            opcode: ((flags >> 11) & 0xf) as u8,
            aa: ((flags >> 10) & 0x1) as u8,
            tc: ((flags >> 9) & 0x1) as u8,
            rd: ((flags >> 8) & 0x1) as u8,
            ra: ((flags >> 7) & 0x1) as u8,
            z: ((flags >> 4) & 0x7) as u8,
            rcode: (flags & 0xf) as u8,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_name(input: &[u8]) -> IResult<&[u8], String> {
    let mut result = String::new();
    let mut current_input = input;
    let mut first = true;

    loop {
        let (input, length) = be_u8(current_input)?;
        if length == 0 {
            return Ok((input, result));
        }

        if !first {
            result.push('.');
        }
        first = false;

        let (input, label) = byte_take(length as usize)(input)?;
        result.push_str(&String::from_utf8_lossy(label));
        current_input = input;
    }
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_name(input)?;
    let (input, qtype) = be_u16(input)?;
    let (input, qclass) = be_u16(input)?;

    Ok((input, DnsQuestion { qname, qtype, qclass }))
}

fn parse_rdata(input: &[u8], type_: u16, length: u16) -> IResult<&[u8], RData> {
    let (input, data) = byte_take(length as usize)(input)?;
    
    match type_ {
        1 => Ok((input, RData::A(data.to_vec()))),
        2 => {
            let (_, name) = parse_name(data)?;
            Ok((input, RData::NS(name)))
        }
        5 => {
            let (_, name) = parse_name(data)?;
            Ok((input, RData::CNAME(name)))
        }
        6 => {
            let (remaining, mname) = parse_name(data)?;
            let (remaining, rname) = parse_name(remaining)?;
            let (_, (serial, refresh, retry, expire, minimum)) =
                tuple((be_u32, be_u32, be_u32, be_u32, be_u32))(remaining)?;
            Ok((
                input,
                RData::SOA {
                    mname,
                    rname,
                    serial,
                    refresh,
                    retry,
                    expire,
                    minimum,
                },
            ))
        }
        12 => {
            let (_, name) = parse_name(data)?;
            Ok((input, RData::PTR(name)))
        }
        15 => {
            let (remaining, preference) = be_u16(data)?;
            let (_, exchange) = parse_name(remaining)?;
            Ok((
                input,
                RData::MX {
                    preference,
                    exchange,
                },
            ))
        }
        16 => {
            let mut texts = Vec::new();
            let mut current_data = data;
            while !current_data.is_empty() {
                let (remaining, length) = be_u8(current_data)?;
                let (remaining, text) = byte_take(length as usize)(remaining)?;
                texts.push(String::from_utf8_lossy(text).to_string());
                current_data = remaining;
            }
            Ok((input, RData::TXT(texts)))
        }
        28 => Ok((input, RData::AAAA(data.to_vec()))),
        _ => Ok((input, RData::Unknown(data.to_vec()))),
    }
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], ResourceRecord> {
    let (input, name) = parse_name(input)?;
    let (input, type_) = be_u16(input)?;
    let (input, class) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = parse_rdata(input, type_, rdlength)?;

    Ok((
        input,
        ResourceRecord {
            name,
            type_,
            class,
            ttl,
            rdata,
        },
    ))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DnsMessage> {
    let (input, header) = parse_header(input)?;
    let (input, questions) = count(parse_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_resource_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_resource_record, header.nscount as usize)(input)?;
    let (input, additionals) = count(parse_resource_record, header.arcount as usize)(input)?;

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

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_binary_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dns_message(&buffer) {
        Ok((remaining, message)) => {
            println!("Parsed DNS message: {:#?}", message);
            if !remaining.is_empty() {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
        }
        Err(e) => eprintln!("Failed to parse DNS message: {:?}", e),
    }

    Ok(())
}