use nom::{
    bits::complete::take,
    bytes::complete::{take as take_bytes},
    error::Error,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

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
    qname: Vec<String>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DnsResourceRecord {
    name: Vec<String>,
    type_: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DnsPacket {
    header: DnsHeader,
    questions: Vec<DnsQuestion>,
    answers: Vec<DnsResourceRecord>,
    authorities: Vec<DnsResourceRecord>,
    additionals: Vec<DnsResourceRecord>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, id) = be_u16(input)?;
    let (input, (
        qr,
        opcode,
        aa,
        tc,
        rd,
        ra,
        z,
        rcode,
    )): (&[u8], (u8, u8, u8, u8, u8, u8, u8, u8)) = nom::bits::bits(tuple((
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(1usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(4usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(1usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(1usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(1usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(1usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(3usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(4usize),
    )))(input)?;
    
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

fn parse_name<'a>(input: &'a [u8], original: &'a [u8]) -> IResult<&'a [u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut current_input = input;

    loop {
        let (input, length) = nom::number::complete::u8(current_input)?;
        
        if length == 0 {
            return Ok((input, labels));
        }

        if length & 0xC0 == 0xC0 {
            let (input, offset_byte) = nom::number::complete::u8(input)?;
            let offset = ((length as u16 & 0x3F) << 8) | offset_byte as u16;
            let (_, pointer_labels) = parse_name(&original[offset as usize..], original)?;
            labels.extend(pointer_labels);
            return Ok((input, labels));
        }

        let (input, label_bytes) = take_bytes(length as usize)(input)?;
        labels.push(String::from_utf8_lossy(label_bytes).to_string());
        current_input = input;
    }
}

fn parse_question<'a>(input: &'a [u8], original: &'a [u8]) -> IResult<&'a [u8], DnsQuestion> {
    let (input, qname) = parse_name(input, original)?;
    let (input, qtype) = be_u16(input)?;
    let (input, qclass) = be_u16(input)?;

    Ok((input, DnsQuestion {
        qname,
        qtype,
        qclass,
    }))
}

fn parse_resource_record<'a>(input: &'a [u8], original: &'a [u8]) -> IResult<&'a [u8], DnsResourceRecord> {
    let (input, name) = parse_name(input, original)?;
    let (input, type_) = be_u16(input)?;
    let (input, class) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take_bytes(rdlength as usize)(input)?;

    Ok((input, DnsResourceRecord {
        name,
        type_,
        class,
        ttl,
        rdlength,
        rdata: rdata.to_vec(),
    }))
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DnsPacket> {
    let original = input;
    let (input, header) = parse_header(input)?;
    
    let mut current_input = input;
    let mut questions = Vec::new();
    let mut answers = Vec::new();
    let mut authorities = Vec::new();
    let mut additionals = Vec::new();

    for _ in 0..header.qdcount {
        let (input, question) = parse_question(current_input, original)?;
        questions.push(question);
        current_input = input;
    }

    for _ in 0..header.ancount {
        let (input, answer) = parse_resource_record(current_input, original)?;
        answers.push(answer);
        current_input = input;
    }

    for _ in 0..header.nscount {
        let (input, authority) = parse_resource_record(current_input, original)?;
        authorities.push(authority);
        current_input = input;
    }

    for _ in 0..header.arcount {
        let (input, additional) = parse_resource_record(current_input, original)?;
        additionals.push(additional);
        current_input = input;
    }

    Ok((current_input, DnsPacket {
        header,
        questions,
        answers,
        authorities,
        additionals,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_packet_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dns_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed DNS packet: {:#?}", packet);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse DNS packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}