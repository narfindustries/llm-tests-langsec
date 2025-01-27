use nom::{
    bytes::complete::{take, take_while},
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
    qname: Vec<u8>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct ResourceRecord {
    name: Vec<u8>,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

#[derive(Debug)]
struct DNSPacket {
    header: DNSHeader,
    questions: Vec<Question>,
    answers: Vec<ResourceRecord>,
    authorities: Vec<ResourceRecord>,
    additionals: Vec<ResourceRecord>,
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
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
            ad,
            cd,
            rcode,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], Question> {
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

fn parse_dns_rr(input: &[u8]) -> IResult<&[u8], ResourceRecord> {
    let (input, name) = take_while(|b: u8| b != 0)(input)?;
    let (input, _) = take(1usize)(input)?; // null byte of name
    let (input, (rtype, rclass, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;

    Ok((
        input,
        ResourceRecord {
            name: name.to_vec(),
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DNSPacket> {
    let (input, header) = parse_dns_header(input)?;
    let mut input = input;
    let mut questions = Vec::new();
    for _ in 0..header.qdcount {
        let (new_input, question) = parse_dns_question(input)?;
        questions.push(question);
        input = new_input;
    }

    let mut answers = Vec::new();
    for _ in 0..header.ancount {
        let (new_input, answer) = parse_dns_rr(input)?;
        answers.push(answer);
        input = new_input;
    }

    let mut authorities = Vec::new();
    for _ in 0..header.nscount {
        let (new_input, authority) = parse_dns_rr(input)?;
        authorities.push(authority);
        input = new_input;
    }

    let mut additionals = Vec::new();
    for _ in 0..header.arcount {
        let (new_input, additional) = parse_dns_rr(input)?;
        additionals.push(additional);
        input = new_input;
    }

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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dns_packet(&buffer) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Failed to parse DNS packet: {:?}", e),
    }

    Ok(())
}