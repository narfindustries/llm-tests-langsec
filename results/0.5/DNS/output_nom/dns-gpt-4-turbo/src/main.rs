use nom::{
    bytes::complete::{take, take_while},
    combinator::{map_res, verify},
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct DNSHeader {
    id: u16,
    qr: u8,
    opcode: u8,
    aa: u8,
    tc: u8,
    rd: u8,
    ra: u8,
    z: u8,
    ad: u8,
    cd: u8,
    rcode: u8,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct Question {
    qname: Vec<String>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct ResourceRecord {
    name: Vec<String>,
    rtype: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, (
        id,
        flags,
        qdcount,
        ancount,
        nscount,
        arcount,
    )) = tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;

    let qr = (flags >> 15) & 0x1;
    let opcode = (flags >> 11) & 0xF;
    let aa = (flags >> 10) & 0x1;
    let tc = (flags >> 9) & 0x1;
    let rd = (flags >> 8) & 0x1;
    let ra = (flags >> 7) & 0x1;
    let z = (flags >> 6) & 0x1;
    let ad = (flags >> 5) & 0x1;
    let cd = (flags >> 4) & 0x1;
    let rcode = flags & 0xF;

    Ok((input, DNSHeader {
        id,
        qr,
        opcode,
        aa,
        tc,
        rd,
        ra,
        z,
        ad,
        cd,
        rcode,
        qdcount,
        ancount,
        nscount,
        arcount,
    }))
}

fn parse_labels(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut input = input;
    let mut labels = Vec::new();

    loop {
        let (next_input, length) = be_u8(input)?;
        if length == 0 {
            break;
        }
        let length = length as usize;
        let (next_input, label) = map_res(take(length), std::str::from_utf8)(next_input)?;
        labels.push(label.to_string());
        input = next_input;
    }

    Ok((input, labels))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], Question> {
    let (input, (qname, qtype, qclass)) = tuple((parse_labels, be_u16, be_u16))(input)?;
    Ok((input, Question { qname, qtype, qclass }))
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], ResourceRecord> {
    let (input, (name, rtype, class, ttl, rdlength)) = tuple((parse_labels, be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((input, ResourceRecord {
        name,
        rtype,
        class,
        ttl,
        rdlength,
        rdata: rdata.to_vec(),
    }))
}

fn parse_dns(input: &[u8]) -> IResult<&[u8], (DNSHeader, Vec<Question>, Vec<ResourceRecord>, Vec<ResourceRecord>, Vec<ResourceRecord>)> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = count(parse_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_resource_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_resource_record, header.nscount as usize)(input)?;
    let (input, additionals) = count(parse_resource_record, header.arcount as usize)(input)?;

    Ok((input, (header, questions, answers, authorities, additionals)))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input file specified"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dns(&buffer) {
        Ok((_, dns)) => {
            println!("{:#?}", dns);
        }
        Err(e) => {
            println!("Failed to parse DNS: {:?}", e);
        }
    }

    Ok(())
}