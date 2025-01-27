use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, rest},
    number::complete::{be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::read;
use std::net::IpAddr;
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
    qname: String,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DnsResourceRecord {
    name: String,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdata: Rdata,
}


#[derive(Debug)]
enum Rdata {
    A(IpAddr),
    AAAA([u8;16]),
    CNAME(String),
    MX(u16,String),
    NS(String),
    SOA(String,String,u32,u32,u32,u32,u32),
    TXT(String),
    // Add other record types as needed...
}


fn parse_domain_name(input: &[u8]) -> IResult<&[u8], String> {
    let mut domain_name = String::new();
    let mut remaining = input;
    loop {
        let (r, len) = be_u8(remaining)?;
        if len == 0 {
            break;
        }
        let (r2, label) = take(len as usize)(r)?;
        domain_name.push_str(from_utf8(label).unwrap());
        domain_name.push('.');
        remaining = r2;
    }
    Ok((remaining, domain_name.trim_end_matches(".").to_string()))
}

fn parse_rdata_a(input: &[u8]) -> IResult<&[u8], Rdata> {
    map(be_u32, |ip| Rdata::A(IpAddr::from(ip.to_be_bytes())))(input)
}

fn parse_rdata_aaaa(input: &[u8]) -> IResult<&[u8], Rdata> {
    map(take(16usize), |ip| Rdata::AAAA(ip.try_into().unwrap()))(input)
}

fn parse_rdata_cname(input: &[u8]) -> IResult<&[u8], Rdata> {
    map(parse_domain_name, |name| Rdata::CNAME(name))(input)
}

fn parse_rdata_mx(input: &[u8]) -> IResult<&[u8], Rdata> {
    map(tuple((be_u16, parse_domain_name)), |(preference, exchange)| Rdata::MX(preference, exchange))(input)
}

fn parse_rdata_ns(input: &[u8]) -> IResult<&[u8], Rdata> {
    map(parse_domain_name, |name| Rdata::NS(name))(input)
}

fn parse_rdata_soa(input: &[u8]) -> IResult<&[u8], Rdata> {
    map(tuple((parse_domain_name, parse_domain_name, be_u32, be_u32, be_u32, be_u32, be_u32)), |(mname, rname, serial, refresh, retry, expire, minimum)| Rdata::SOA(mname, rname, serial, refresh, retry, expire, minimum))(input)
}

fn parse_rdata_txt(input: &[u8]) -> IResult<&[u8], Rdata> {
    map(rest, |bytes| {
        let text = from_utf8(bytes).unwrap();
        Rdata::TXT(text.to_string())
    })(input)
}


fn parse_rdata(rtype: u16, input: &[u8]) -> IResult<&[u8], Rdata> {
    match rtype {
        1 => parse_rdata_a(input),
        28 => parse_rdata_aaaa(input),
        5 => parse_rdata_cname(input),
        15 => parse_rdata_mx(input),
        2 => parse_rdata_ns(input),
        6 => parse_rdata_soa(input),
        16 => parse_rdata_txt(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}


fn parse_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    map(tuple((parse_domain_name, be_u16, be_u16, be_u32, parse_rdata)), |(name, rtype, rclass, ttl, rdata)| {
        DnsResourceRecord { name, rtype, rclass, ttl, rdata }
    })(input)
}


fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    map(tuple((parse_domain_name, be_u16, be_u16)), |(qname, qtype, qclass)| DnsQuestion { qname, qtype, qclass })(input)
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    map(tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16, be_u16)),
        |(id, flags, qdcount, ancount, nscount, arcount)| {
            let qr = (flags >> 15) & 1 != 0;
            let opcode = ((flags >> 11) & 0b1111) as u8;
            let aa = (flags >> 10) & 1 != 0;
            let tc = (flags >> 9) & 1 != 0;
            let rd = (flags >> 8) & 1 != 0;
            let ra = (flags >> 7) & 1 != 0;
            let rcode = (flags & 0b1111) as u8;
            DnsHeader { id, qr, opcode, aa, tc, rd, ra, rcode, qdcount, ancount, nscount, arcount }
    })(input)
}

fn be_u8(i: &[u8]) -> IResult<&[u8], u8> {
    map(take(1usize), |x| x[0])(i)
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = match read(filename) {
        Ok(data) => data,
        Err(e) => {
            println!("Error reading file: {}", e);
            return;
        }
    };

    match parse_dns_header(&data) {
        Ok((remaining, header)) => {
            println!("DNS Header: {:?}", header);
            let mut remaining_data = remaining;
            for _ in 0..header.qdcount {
                match parse_dns_question(remaining_data) {
                    Ok((r, question)) => {
                        println!("DNS Question: {:?}", question);
                        remaining_data = r;
                    },
                    Err(e) => {
                        println!("Error parsing question: {:?}", e);
                        break;
                    }
                }
            }

            for _ in 0..header.ancount {
                match parse_resource_record(remaining_data) {
                    Ok((r, rr)) => {
                        println!("Resource Record: {:?}", rr);
                        remaining_data = r;
                    }
                    Err(e) => {
                        println!("Error parsing resource record: {:?}", e);
                        break;
                    }
                }
            }

            //parse NS, AR records similarly.

        }
        Err(e) => println!("Error parsing header: {:?}", e),
    }
}
