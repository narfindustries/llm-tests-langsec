use nom::{
    bits::complete::{tag, take},
    branch::alt,
    bytes::complete::{take as take_bytes},
    combinator::{map, verify},
    multi::{count, length_count},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct DnsHeader {
    id: u16,
    qr: bool,
    opcode: u8,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
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
enum RData {
    A(u32),
    NS(Vec<String>),
    CNAME(Vec<String>),
    SOA {
        mname: Vec<String>,
        rname: Vec<String>,
        serial: u32,
        refresh: u32,
        retry: u32,
        expire: u32,
        minimum: u32,
    },
    PTR(Vec<String>),
    MX {
        preference: u16,
        exchange: Vec<String>,
    },
    TXT(String),
    AAAA([u8; 16]),
    Unknown(Vec<u8>),
}

#[derive(Debug)]
struct DnsResourceRecord {
    name: Vec<String>,
    type_: u16,
    class: u16,
    ttl: u32,
    rdata: RData,
}

#[derive(Debug)]
struct DnsMessage {
    header: DnsHeader,
    questions: Vec<DnsQuestion>,
    answers: Vec<DnsResourceRecord>,
    authorities: Vec<DnsResourceRecord>,
    additionals: Vec<DnsResourceRecord>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, id) = be_u16(input)?;
    let (input, flags) = be_u16(input)?;
    let (input, (qdcount, ancount, nscount, arcount)) = tuple((be_u16, be_u16, be_u16, be_u16))(input)?;

    Ok((
        input,
        DnsHeader {
            id,
            qr: (flags & 0x8000) != 0,
            opcode: ((flags & 0x7800) >> 11) as u8,
            aa: (flags & 0x0400) != 0,
            tc: (flags & 0x0200) != 0,
            rd: (flags & 0x0100) != 0,
            ra: (flags & 0x0080) != 0,
            z: ((flags & 0x0070) >> 4) as u8,
            rcode: (flags & 0x000F) as u8,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut result = Vec::new();
    let mut remaining = input;

    loop {
        let (input, length) = be_u8(remaining)?;
        
        if length == 0 {
            return Ok((input, result));
        }

        if (length & 0xC0) == 0xC0 {
            // Compression pointer
            let (input, offset) = be_u8(input)?;
            let pointer = ((length as u16 & 0x3F) << 8) | offset as u16;
            // Note: Compression pointers need to be handled with the original message buffer
            return Ok((input, result));
        }

        let (input, label) = take_bytes(length as usize)(input)?;
        result.push(String::from_utf8_lossy(label).to_string());
        remaining = input;
    }
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_name(input)?;
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

fn parse_rdata(input: &[u8], type_: u16, length: u16) -> IResult<&[u8], RData> {
    match type_ {
        1 => {
            // A Record
            let (input, addr) = be_u32(input)?;
            Ok((input, RData::A(addr)))
        }
        2 => {
            // NS Record
            let (input, name) = parse_name(input)?;
            Ok((input, RData::NS(name)))
        }
        5 => {
            // CNAME Record
            let (input, name) = parse_name(input)?;
            Ok((input, RData::CNAME(name)))
        }
        6 => {
            // SOA Record
            let (input, mname) = parse_name(input)?;
            let (input, rname) = parse_name(input)?;
            let (input, (serial, refresh, retry, expire, minimum)) =
                tuple((be_u32, be_u32, be_u32, be_u32, be_u32))(input)?;
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
            // PTR Record
            let (input, name) = parse_name(input)?;
            Ok((input, RData::PTR(name)))
        }
        15 => {
            // MX Record
            let (input, preference) = be_u16(input)?;
            let (input, exchange) = parse_name(input)?;
            Ok((
                input,
                RData::MX {
                    preference,
                    exchange,
                },
            ))
        }
        16 => {
            // TXT Record
            let (input, data) = take_bytes(length as usize)(input)?;
            Ok((input, RData::TXT(String::from_utf8_lossy(data).to_string())))
        }
        28 => {
            // AAAA Record
            let (input, data) = take_bytes(16)(input)?;
            let mut addr = [0u8; 16];
            addr.copy_from_slice(data);
            Ok((input, RData::AAAA(addr)))
        }
        _ => {
            // Unknown Record Type
            let (input, data) = take_bytes(length as usize)(input)?;
            Ok((input, RData::Unknown(data.to_vec())))
        }
    }
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    let (input, name) = parse_name(input)?;
    let (input, type_) = be_u16(input)?;
    let (input, class) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = parse_rdata(input, type_, rdlength)?;

    Ok((
        input,
        DnsResourceRecord {
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
        eprintln!("Usage: {} <dns_message_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dns_message(&buffer) {
        Ok((remaining, message)) => {
            println!("Parsed DNS message: {:#?}", message);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining unparsed", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Failed to parse DNS message: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}