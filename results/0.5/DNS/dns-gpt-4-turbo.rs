use nom::{
    bytes::complete::{take},
    multi::{count},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

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
    authority: Vec<ResourceRecord>,
    additional: Vec<ResourceRecord>,
}

fn parse_name(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u8(input)?;
    if length == 0 {
        return Ok((input, vec![]));
    }
    let (input, name) = take(length)(input)?;
    let (input, mut subname) = parse_name(input)?;
    let mut full_name = vec![length];
    full_name.extend_from_slice(name);
    full_name.append(&mut subname);
    Ok((input, full_name))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
    let qr = (flags >> 15) & 0x1;
    let opcode = (flags >> 11) & 0xF;
    let aa = (flags >> 10) & 0x1;
    let tc = (flags >> 9) & 0x1;
    let rd = (flags >> 8) & 0x1;
    let ra = (flags >> 7) & 0x1;
    let z = (flags >> 4) & 0x7;
    let rcode = flags & 0xF;
    Ok((
        input,
        Header {
            id,
            qr: qr as u8,
            opcode: opcode as u8,
            aa: aa as u8,
            tc: tc as u8,
            rd: rd as u8,
            ra: ra as u8,
            z: z as u8,
            rcode: rcode as u8,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], Question> {
    let (input, qname) = parse_name(input)?;
    let (input, (qtype, qclass)) = tuple((be_u16, be_u16))(input)?;
    Ok((input, Question { qname, qtype, qclass }))
}

fn parse_rr(input: &[u8]) -> IResult<&[u8], ResourceRecord> {
    let (input, name) = parse_name(input)?;
    let (input, (rtype, class, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((
        input,
        ResourceRecord {
            name,
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
    let (input, authority) = count(parse_rr, header.nscount as usize)(input)?;
    let (input, additional) = count(parse_rr, header.arcount as usize)(input)?;
    Ok((
        input,
        DnsPacket {
            header,
            questions,
            answers,
            authority,
            additional,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input file specified"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_dns_packet(&buffer) {
        Ok((_, packet)) => {
            println!("{:#?}", packet);
        }
        Err(e) => {
            println!("Failed to parse DNS packet: {:?}", e);
        }
    }

    Ok(())
}