use nom::{
    bits::complete::take,
    bytes::complete::{take as take_bytes},
    multi::count,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
    error::Error,
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
    )): (&[u8], (u8, u8, u8, u8, u8, u8, u8, u8)) = 
    nom::bits::bits(tuple((
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(1usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(4usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(1usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(1usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(1usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(1usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(3usize),
        take::<&[u8], u8, usize, Error<(&[u8], usize)>>(4usize),
    )))(input)?;
    
    let (input, (qdcount, ancount, nscount, arcount)) = 
        tuple((be_u16, be_u16, be_u16, be_u16))(input)?;

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

fn parse_name(input: &[u8]) -> IResult<&[u8], String> {
    let mut result = String::new();
    let mut current_input = input;
    let mut first = true;

    loop {
        let (input, length) = nom::number::complete::u8(current_input)?;
        if length == 0 {
            current_input = input;
            break;
        }

        if !first {
            result.push('.');
        }
        first = false;

        let (input, label) = take_bytes(length as usize)(input)?;
        result.push_str(&String::from_utf8_lossy(label));
        current_input = input;
    }

    Ok((current_input, result))
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
    let (input, (type_, class, ttl, rdlength)) = 
        tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
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
    let (input, header) = parse_header(input)?;
    let (input, questions) = count(parse_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_resource_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_resource_record, header.nscount as usize)(input)?;
    let (input, additionals) = count(parse_resource_record, header.arcount as usize)(input)?;

    Ok((input, DnsPacket {
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
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Failed to parse DNS packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}