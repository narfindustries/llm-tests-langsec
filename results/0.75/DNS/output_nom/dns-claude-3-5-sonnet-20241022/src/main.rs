use nom::{
    bits::{bits, complete::take},
    bytes::complete::{tag, take as take_bytes},
    combinator::{map, verify},
    multi::{count, length_count},
    number::complete::{be_u16, be_u32},
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
struct DnsResourceRecord {
    name: Vec<String>,
    rtype: u16,
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
    )): (_, (u8, u8, u8, u8, u8, u8, u8, u8)) = bits::<_, _, nom::error::Error<(&[u8], usize)>, _, _>(tuple((
        take(1usize),
        take(4usize),
        take(1usize),
        take(1usize),
        take(1usize),
        take(1usize),
        take(3usize),
        take(4usize),
    )))(input)?;

    let (input, (qdcount, ancount, nscount, arcount)) = tuple((
        be_u16,
        be_u16,
        be_u16,
        be_u16,
    ))(input)?;

    Ok((input, DnsHeader {
        id,
        qr: qr == 1,
        opcode,
        aa: aa == 1,
        tc: tc == 1,
        rd: rd == 1,
        ra: ra == 1,
        z,
        rcode,
        qdcount,
        ancount,
        nscount,
        arcount,
    }))
}

fn parse_label(input: &[u8]) -> IResult<&[u8], String> {
    let (input, length) = verify(nom::number::complete::u8, |&x| x <= 63)(input)?;
    if length == 0 {
        return Ok((input, String::new()));
    }
    let (input, label_bytes) = take_bytes(length as usize)(input)?;
    Ok((input, String::from_utf8_lossy(label_bytes).into_owned()))
}

fn parse_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut labels = Vec::new();
    let mut remaining = input;

    loop {
        let (input, label) = parse_label(remaining)?;
        if label.is_empty() {
            return Ok((input, labels));
        }
        labels.push(label);
        remaining = input;
    }
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_name(input)?;
    let (input, (qtype, qclass)) = tuple((be_u16, be_u16))(input)?;

    Ok((input, DnsQuestion {
        qname,
        qtype,
        qclass,
    }))
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    let (input, name) = parse_name(input)?;
    let (input, (rtype, class, ttl, rdlength)) = tuple((
        be_u16,
        be_u16,
        be_u32,
        be_u16,
    ))(input)?;
    let (input, rdata) = take_bytes(rdlength as usize)(input)?;

    Ok((input, DnsResourceRecord {
        name,
        rtype,
        class,
        ttl,
        rdlength,
        rdata: rdata.to_vec(),
    }))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], DnsMessage> {
    let (input, header) = parse_header(input)?;
    let (input, questions) = count(parse_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_resource_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_resource_record, header.nscount as usize)(input)?;
    let (input, additionals) = count(parse_resource_record, header.arcount as usize)(input)?;

    Ok((input, DnsMessage {
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
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
        }
        Err(e) => eprintln!("Failed to parse DNS message: {}", e),
    }

    Ok(())
}