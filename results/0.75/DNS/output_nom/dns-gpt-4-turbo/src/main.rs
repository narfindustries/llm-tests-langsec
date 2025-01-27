use nom::{
    bytes::complete::{take, take_while},
    combinator::{map_res, verify},
    multi::{count, many0},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{env, fs::File, io::Read, net::Ipv4Addr};

#[derive(Debug)]
enum QueryType {
    A,
    NS,
    CNAME,
    SOA,
    PTR,
    MX,
    TXT,
    AAAA,
    SRV,
    Other(u16),
}

#[derive(Debug)]
struct DNSHeader {
    id: u16,
    qr: u8,
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
struct DNSQuestion {
    qname: Vec<String>,
    qtype: QueryType,
    qclass: u16,
}

#[derive(Debug)]
struct DNSRecord {
    name: Vec<String>,
    rtype: QueryType,
    rclass: u16,
    ttl: u32,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DNSPacket {
    header: DNSHeader,
    questions: Vec<DNSQuestion>,
    answers: Vec<DNSRecord>,
    authorities: Vec<DNSRecord>,
    additionals: Vec<DNSRecord>,
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, id) = be_u16(input)?;
    let (input, flags) = be_u16(input)?;
    let (input, qdcount) = be_u16(input)?;
    let (input, ancount) = be_u16(input)?;
    let (input, nscount) = be_u16(input)?;
    let (input, arcount) = be_u16(input)?;

    Ok((
        input,
        DNSHeader {
            id,
            qr: ((flags >> 15) & 0x01) as u8,
            opcode: ((flags >> 11) & 0x0F) as u8,
            aa: (flags & (1 << 10)) > 0,
            tc: (flags & (1 << 9)) > 0,
            rd: (flags & (1 << 8)) > 0,
            ra: (flags & (1 << 7)) > 0,
            z: ((flags >> 4) & 0x07) as u8,
            rcode: (flags & 0x0F) as u8,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (input, qname) = parse_qname(input)?;
    let (input, qtype) = be_u16(input)?;
    let (input, qclass) = be_u16(input)?;

    let qtype = match qtype {
        1 => QueryType::A,
        2 => QueryType::NS,
        5 => QueryType::CNAME,
        6 => QueryType::SOA,
        12 => QueryType::PTR,
        15 => QueryType::MX,
        16 => QueryType::TXT,
        28 => QueryType::AAAA,
        33 => QueryType::SRV,
        _ => QueryType::Other(qtype),
    };

    Ok((
        input,
        DNSQuestion {
            qname,
            qtype,
            qclass,
        },
    ))
}

fn parse_qname(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut input = input;
    let mut parts = vec![];

    loop {
        let (next_input, len) = be_u8(input)?;
        if len == 0 {
            break;
        }

        let (next_input, part) = take(len)(next_input)?;
        parts.push(String::from_utf8_lossy(part).to_string());
        input = next_input;
    }

    Ok((input, parts))
}

fn parse_dns_record(input: &[u8]) -> IResult<&[u8], DNSRecord> {
    let (input, name) = parse_qname(input)?;
    let (input, rtype) = be_u16(input)?;
    let (input, rclass) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength)(input)?;

    let rtype = match rtype {
        1 => QueryType::A,
        2 => QueryType::NS,
        5 => QueryType::CNAME,
        6 => QueryType::SOA,
        12 => QueryType::PTR,
        15 => QueryType::MX,
        16 => QueryType::TXT,
        28 => QueryType::AAAA,
        33 => QueryType::SRV,
        _ => QueryType::Other(rtype),
    };

    Ok((
        input,
        DNSRecord {
            name,
            rtype,
            rclass,
            ttl,
            rdata: rdata.to_vec(),
        },
    ))
}

fn read_dns_packet(input: &[u8]) -> IResult<&[u8], DNSPacket> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = count(parse_dns_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_dns_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_dns_record, header.nscount as usize)(input)?;
    let (input, additionals) = count(parse_dns_record, header.arcount as usize)(input)?;

    Ok((
        input,
        DNSPacket {
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
    if args.len() < 2 {
        println!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file.");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file.");

    match read_dns_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => println!("Failed to parse DNS packet: {:?}", e),
    }
}