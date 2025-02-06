use nom::{
    bytes::complete::{take},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
    error::Error,
    multi::count,
    branch::alt,
    combinator::rest
};
use std::fs;
use std::str::from_utf8;

#[derive(Debug)]
struct DnsHeader {
    id: u16,
    qr: bool,
    opcode: u8,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    rcode: u8,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct DnsQuestion {
    name: String,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DnsResourceRecord {
    name: String,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}


fn parse_domain_name(input: &[u8]) -> IResult<&[u8], String, Error<&[u8]>> {
    let mut domain_name = String::new();
    let mut remaining = input;
    loop {
        let (rest, len) = take(1usize)(remaining)?;
        let len = len[0];
        if len == 0 {
            break;
        }
        let (rest2, label) = take(len as usize)(rest)?;
        domain_name.push_str(from_utf8(label).unwrap());
        domain_name.push('.');
        remaining = rest2;
    }
    Ok((remaining, domain_name))
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader, Error<&[u8]>> {
    let (rest, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
    ))(input)?;

    let qr = (flags >> 15) & 1 != 0;
    let opcode = ((flags >> 11) & 0xf) as u8;
    let aa = (flags >> 10) & 1 != 0;
    let tc = (flags >> 9) & 1 != 0;
    let rd = (flags >> 8) & 1 != 0;
    let ra = (flags >> 7) & 1 != 0;
    let rcode = (flags & 0xf) as u8;

    Ok((
        rest,
        DnsHeader {
            id,
            qr,
            opcode,
            aa,
            tc,
            rd,
            ra,
            rcode,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion, Error<&[u8]>> {
    let (rest, (name, qtype, qclass)) = tuple((parse_domain_name, be_u16, be_u16))(input)?;
    Ok((
        rest,
        DnsQuestion {
            name: name.trim_end_matches('.').to_string(),
            qtype,
            qclass,
        },
    ))
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord, Error<&[u8]>> {
    let (rest, (name, rtype, rclass, ttl, rdlength, rdata)) = tuple((
        parse_domain_name,
        be_u16,
        be_u16,
        be_u32,
        be_u16,
        take,
    ))(input)?;
    Ok((
        rest,
        DnsResourceRecord {
            name: name.trim_end_matches('.').to_string(),
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], (DnsHeader, Vec<DnsQuestion>, Vec<DnsResourceRecord>, Vec<DnsResourceRecord>, Vec<DnsResourceRecord>), Error<&[u8]>> {
    let (rest, header) = parse_dns_header(input)?;
    let (rest, questions) = count(parse_dns_question, header.qdcount as usize)(rest)?;
    let (rest, answers) = count(parse_dns_resource_record, header.ancount as usize)(rest)?;
    let (rest, authorities) = count(parse_dns_resource_record, header.nscount as usize)(rest)?;
    let (rest, additionals) = count(parse_dns_resource_record, header.arcount as usize)(rest)?;
    Ok((rest, (header, questions, answers, authorities, additionals)))

}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Something went wrong reading the file");

    match parse_dns_message(&contents) {
        Ok((_, (header, questions, answers, authorities, additionals))) => {
            println!("Header: {:?}", header);
            println!("Questions: {:?}", questions);
            println!("Answers: {:?}", answers);
            println!("Authorities: {:?}", authorities);
            println!("Additionals: {:?}", additionals);
        }
        Err(e) => {
            eprintln!("Error parsing DNS message: {:?}", e);
            std::process::exit(1);
        }
    }
}
