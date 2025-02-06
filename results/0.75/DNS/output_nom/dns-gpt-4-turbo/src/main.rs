use nom::{
    bytes::complete::{take, take_while},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};
use std::convert::TryInto;

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
    rcode: u8,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct DNSQuestion {
    qname: Vec<u8>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DNSRecord {
    name: Vec<u8>,
    type_: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
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
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;

    let qr = ((flags >> 15) & 0x1) as u8;
    let opcode = ((flags >> 11) & 0xF) as u8;
    let aa = ((flags >> 10) & 0x1) as u8;
    let tc = ((flags >> 9) & 0x1) as u8;
    let rd = ((flags >> 8) & 0x1) as u8;
    let ra = ((flags >> 7) & 0x1) as u8;
    let z = ((flags >> 4) & 0x7) as u8;
    let rcode = (flags & 0xF) as u8;

    Ok((
        input,
        DNSHeader {
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

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (input, qname) = take_while(|b: u8| b != 0)(input)?;
    let (input, _) = take(1usize)(input)?; // Null byte at the end of the qname
    let (input, (qtype, qclass)) = tuple((be_u16, be_u16))(input)?;

    Ok((
        input,
        DNSQuestion {
            qname: qname.to_vec(),
            qtype,
            qclass,
        },
    ))
}

fn parse_dns_record(input: &[u8]) -> IResult<&[u8], DNSRecord> {
    let (input, name) = take_while(|b: u8| b != 0)(input)?;
    let (input, _) = take(1usize)(input)?; // Null byte at the end of the name
    let (input, (type_, class, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;

    Ok((
        input,
        DNSRecord {
            name: name.to_vec(),
            type_,
            class,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DNSPacket> {
    let (input, header) = parse_dns_header(input)?;
    let mut input_remaining = input;

    let mut questions = Vec::with_capacity(header.qdcount as usize);
    for _ in 0..header.qdcount {
        let (input_new, question) = parse_dns_question(input_remaining)?;
        input_remaining = input_new;
        questions.push(question);
    }

    let mut answers = Vec::with_capacity(header.ancount as usize);
    for _ in 0..header.ancount {
        let (input_new, record) = parse_dns_record(input_remaining)?;
        input_remaining = input_new;
        answers.push(record);
    }

    let mut authorities = Vec::with_capacity(header.nscount as usize);
    for _ in 0..header.nscount {
        let (input_new, record) = parse_dns_record(input_remaining)?;
        input_remaining = input_new;
        authorities.push(record);
    }

    let mut additionals = Vec::with_capacity(header.arcount as usize);
    for _ in 0..header.arcount {
        let (input_new, record) = parse_dns_record(input_remaining)?;
        input_remaining = input_new;
        additionals.push(record);
    }

    Ok((
        input_remaining,
        DNSPacket {
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
    if args.len() < 2 {
        println!("Usage: {} <path_to_dns_packet_file>", args[0]);
        return Ok(());
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