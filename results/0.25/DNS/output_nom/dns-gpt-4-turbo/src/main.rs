use nom::{
    bytes::complete::{take, take_while},
    multi::count,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::convert::TryInto;

#[derive(Debug)]
struct Header {
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
struct Question {
    qname: Vec<u8>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct ResourceRecord {
    name: Vec<u8>,
    rtype: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DnsPacket {
    header: Header,
    questions: Vec<Question>,
    answers: Vec<ResourceRecord>,
    authorities: Vec<ResourceRecord>,
    additionals: Vec<ResourceRecord>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
    let qr = ((flags >> 15) & 0x01) as u8;
    let opcode = ((flags >> 11) & 0x0F) as u8;
    let aa = ((flags >> 10) & 0x01) as u8;
    let tc = ((flags >> 9) & 0x01) as u8;
    let rd = ((flags >> 8) & 0x01) as u8;
    let ra = ((flags >> 7) & 0x01) as u8;
    let z = ((flags >> 4) & 0x07) as u8;
    let rcode = (flags & 0x0F) as u8;
    Ok((
        input,
        Header {
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
        },
    ))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], Question> {
    let (input, qname) = take_while(|b: u8| b != 0)(input)?;
    let (input, _) = take(1usize)(input)?; // null byte of qname
    let (input, (qtype, qclass)) = tuple((be_u16, be_u16))(input)?;
    Ok((
        input,
        Question {
            qname: qname.to_vec(),
            qtype,
            qclass,
        },
    ))
}

fn parse_rr(input: &[u8]) -> IResult<&[u8], ResourceRecord> {
    let (input, name) = take_while(|b: u8| b != 0)(input)?;
    let (input, _) = take(1usize)(input)?; // null byte of name
    let (input, (rtype, class, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((
        input,
        ResourceRecord {
            name: name.to_vec(),
            rtype,
            class,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DnsPacket> {
    let (input, header) = parse_header(input)?;
    let (input, questions) = count(parse_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_rr, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_rr, header.nscount as usize)(input)?;
    let (input, additionals) = count(parse_rr, header.arcount as usize)(input)?;
    Ok((
        input,
        DnsPacket {
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
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Usage: dns_parser <file>"));
    }
    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dns_packet(&buffer) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => println!("Failed to parse DNS packet: {:?}", e),
    }

    Ok(())
}