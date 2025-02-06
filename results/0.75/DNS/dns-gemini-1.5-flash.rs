use nom::{
    bytes::complete::{take},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
    error::Error,
};
use std::fs;
use std::net::{Ipv4Addr, Ipv6Addr};
use std::convert::TryInto;

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
struct DnsQuestion {
    qname: String,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug, PartialEq)]
struct DnsResourceRecord {
    name: String,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Rdata,
}

#[derive(Debug, PartialEq)]
enum Rdata {
    A(Ipv4Addr),
    AAAA(Ipv6Addr),
    Unknown(Vec<u8>),
}

fn parse_domain_name(input: &[u8]) -> IResult<&[u8], String, Error<&[u8]>> {
    let mut domain_name = String::new();
    let mut remaining = input;
    loop {
        let (rest, len) = be_u8(remaining)?;
        if len == 0 {
            break;
        }
        let (rest2, label) = take(len as usize)(rest)?;
        domain_name.push_str(&String::from_utf8_lossy(label));
        domain_name.push('.');
        remaining = rest2;
    }
    Ok((remaining, domain_name.trim_end_matches('.').to_string()))
}


fn parse_header(input: &[u8]) -> IResult<&[u8], DnsHeader, Error<&[u8]>> {
    let (rest, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((
        be_u16, be_u16, be_u16, be_u16, be_u16, be_u16,
    ))(input)?;

    let qr = (flags >> 15) & 1 != 0;
    let opcode = (flags >> 11) & 0xF;
    let aa = (flags >> 10) & 1 != 0;
    let tc = (flags >> 9) & 1 != 0;
    let rd = (flags >> 8) & 1 != 0;
    let ra = (flags >> 7) & 1 != 0;
    let z = (flags >> 4) & 7;
    let rcode = flags & 0xF;

    Ok((
        rest,
        DnsHeader {
            id,
            qr,
            opcode: opcode as u8,
            aa,
            tc,
            rd,
            ra,
            z: z as u8,
            rcode: rcode as u8,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DnsQuestion, Error<&[u8]>> {
    let (rest, (qname, qtype, qclass)) = tuple((parse_domain_name, be_u16, be_u16))(input)?;
    Ok((rest, DnsQuestion { qname, qtype, qclass }))
}

fn parse_rdata_a(input: &[u8]) -> IResult<&[u8], Rdata, Error<&[u8]>> {
    map(be_u32, |ip| Rdata::A(Ipv4Addr::from(ip)))(input)
}

fn parse_rdata_aaaa(input: &[u8]) -> IResult<&[u8], Rdata, Error<&[u8]>> {
    map_res(take(16usize), |bytes: &[u8]| {
        bytes.try_into().map(Ipv6Addr::from).map(Rdata::AAAA)
    })(input)
}

fn parse_rdata(input: &[u8], rdlength: u16, rtype: u16) -> IResult<&[u8], Rdata, Error<&[u8]>> {
    match rtype {
        1 => parse_rdata_a(input),
        28 => parse_rdata_aaaa(input),
        _ => map(take(rdlength as usize), |bytes: &[u8]| Rdata::Unknown(bytes.to_vec()))(input),
    }
}


fn parse_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord, Error<&[u8]>> {
    let (rest, (name, rtype, rclass, ttl, rdlength)) = tuple((
        parse_domain_name,
        be_u16,
        be_u16,
        be_u32,
        be_u16,
    ))(input)?;
    let (rest, rdata) = parse_rdata(rest, rdlength, rtype)?;
    Ok((
        rest,
        DnsResourceRecord {
            name,
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: dns_parser <binary_file>");
        return;
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Failed to read file");

    let result = parse_header(&contents);
    match result {
        Ok((remaining, header)) => {
            println!("Header: {:?}", header);
            let mut remaining_questions = remaining;
            for _ in 0..header.qdcount {
                let (new_remaining, question) = parse_question(remaining_questions).unwrap();
                println!("Question: {:?}", question);
                remaining_questions = new_remaining;
            }
            let mut remaining_answers = remaining_questions;
            for _ in 0..header.ancount {
                let (new_remaining, answer) = parse_resource_record(remaining_answers).unwrap();
                println!("Answer: {:?}", answer);
                remaining_answers = new_remaining;
            }
            //Process NS and AR records similarly.

        }
        Err(e) => println!("Error parsing header: {:?}", e),
    }
}
