use std::env;
use std::fs;
use nom::{
    IResult,
    bytes::complete::take,
    number::complete::{be_u8, be_u16, be_u32},
    sequence::tuple,
};

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
    qname: String,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DnsResourceRecord {
    name: String,
    type_: u16,
    class: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DnsMessage {
    header: DnsHeader,
    questions: Vec<DnsQuestion>,
    answers: Vec<DnsResourceRecord>,
    authorities: Vec<DnsResourceRecord>,
    additionals: Vec<DnsResourceRecord>,
}

fn parse_header<'a>(input: &'a [u8]) -> IResult<&'a [u8], DnsHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
    ))(input)?;

    let qr = (flags >> 15) & 1 == 1;
    let opcode = ((flags >> 11) & 0xF) as u8;
    let aa = (flags >> 10) & 1 == 1;
    let tc = (flags >> 9) & 1 == 1;
    let rd = (flags >> 8) & 1 == 1;
    let ra = (flags >> 7) & 1 == 1;
    let z = ((flags >> 4) & 0x7) as u8;
    let rcode = (flags & 0xF) as u8;

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

fn parse_name<'a>(input: &'a [u8], message: &'a [u8]) -> IResult<&'a [u8], String> {
    let mut result = String::new();
    let mut current_input = input;
    let mut first = true;

    loop {
        let (new_input, length) = be_u8(current_input)?;
        if length == 0 {
            return Ok((new_input, result));
        }

        if length & 0xC0 == 0xC0 {
            let (new_input, offset_byte) = be_u8(new_input)?;
            let offset = (((length & 0x3F) as u16) << 8) | offset_byte as u16;
            let (_, pointed_name) = parse_name(&message[offset as usize..], message)?;
            if !first {
                result.push('.');
            }
            result.push_str(&pointed_name);
            return Ok((new_input, result));
        }

        let (new_input, label) = take(length as usize)(new_input)?;
        if !first {
            result.push('.');
        }
        result.push_str(&String::from_utf8_lossy(label));
        first = false;
        current_input = new_input;
    }
}

fn parse_question<'a>(input: &'a [u8], message: &'a [u8]) -> IResult<&'a [u8], DnsQuestion> {
    let (input, name) = parse_name(input, message)?;
    let (input, (qtype, qclass)) = tuple((be_u16, be_u16))(input)?;

    Ok((input, DnsQuestion {
        qname: name,
        qtype,
        qclass,
    }))
}

fn parse_resource_record<'a>(input: &'a [u8], message: &'a [u8]) -> IResult<&'a [u8], DnsResourceRecord> {
    let (input, name) = parse_name(input, message)?;
    let (input, (type_, class, ttl, rdlength)) = tuple((
        be_u16,
        be_u16,
        be_u32,
        be_u16,
    ))(input)?;

    let (input, rdata) = take(rdlength as usize)(input)?;

    Ok((input, DnsResourceRecord {
        name,
        type_,
        class,
        ttl,
        rdlength,
        rdata: rdata.to_vec(),
    }))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DnsMessage> {
    let original_input = input;
    let (input, header) = parse_header(input)?;

    let mut current_input = input;
    let mut questions = Vec::new();
    let mut answers = Vec::new();
    let mut authorities = Vec::new();
    let mut additionals = Vec::new();

    for _ in 0..header.qdcount {
        let (new_input, question) = parse_question(current_input, original_input)?;
        questions.push(question);
        current_input = new_input;
    }

    for _ in 0..header.ancount {
        let (new_input, answer) = parse_resource_record(current_input, original_input)?;
        answers.push(answer);
        current_input = new_input;
    }

    for _ in 0..header.nscount {
        let (new_input, authority) = parse_resource_record(current_input, original_input)?;
        authorities.push(authority);
        current_input = new_input;
    }

    for _ in 0..header.arcount {
        let (new_input, additional) = parse_resource_record(current_input, original_input)?;
        additionals.push(additional);
        current_input = new_input;
    }

    Ok((current_input, DnsMessage {
        header,
        questions,
        answers,
        authorities,
        additionals,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns_binary_file>", args[0]);
        std::process::exit(1);
    }

    let data = fs::read(&args[1]).expect("Failed to read file");
    match parse_dns_message(&data) {
        Ok((remaining, message)) => {
            println!("Parsed DNS message: {:#?}", message);
            if !remaining.is_empty() {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
        }
        Err(e) => eprintln!("Failed to parse DNS message: {:?}", e),
    }
}