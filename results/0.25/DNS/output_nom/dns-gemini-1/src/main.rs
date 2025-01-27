use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, verify},
    number::streaming::{be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::read;
use std::net::IpAddr;
use std::str::from_utf8;

#[derive(Debug)]
enum RdataType {
    A,
    AAAA,
    CNAME,
    MX,
    NS,
    SOA,
    TXT,
    // Add other RDATA types as needed
}

#[derive(Debug)]
struct DNSHeader {
    id: u16,
    flags: u16,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct DNSQuestion {
    qname: String,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DNSResourceRecord {
    name: String,
    rtype: RdataType,
    rclass: u16,
    ttl: u32,
    rdata: Vec<u8>,
}


fn parse_name(input: &[u8]) -> IResult<&[u8], String> {
    let mut name = String::new();
    let mut remaining = input;
    loop {
        let (rest, len) = take(1usize)(remaining)?;
        let len = len[0] as usize;
        if len == 0 {
            break;
        }
        let (rest2, label) = take(len)(rest)?;
        name.push_str(from_utf8(label).unwrap());
        name.push('.');
        remaining = rest2;
    }
    Ok((remaining, name.trim_end_matches('.').to_string()))
}

fn parse_rdata_a(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take(4usize), |x| x.to_vec())(input)
}

fn parse_rdata_aaaa(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    map(take(16usize), |x| x.to_vec())(input)
}

fn parse_rdata_cname(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (rest, name) = parse_name(input)?;
    Ok((rest, name.as_bytes().to_vec()))
}

fn parse_rdata(rtype: RdataType, input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    match rtype {
        RdataType::A => parse_rdata_a(input),
        RdataType::AAAA => parse_rdata_aaaa(input),
        RdataType::CNAME => parse_rdata_cname(input),
        _ => take(0usize)(input), // Handle other types later
    }
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    map(
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16)),
        |(id, flags, qdcount, ancount, nscount, arcount)| DNSHeader {
            id,
            flags,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    )(input)
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    map(tuple((parse_name, be_u16, be_u16)), |(qname, qtype, qclass)| DNSQuestion {
        qname,
        qtype,
        qclass,
    })(input)
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    map(
        tuple((
            parse_name,
            be_u16,
            be_u16,
            be_u32,
            // Add RDATA parsing based on type
            map(be_u16, |rtype| match rtype {
                1 => RdataType::A,
                28 => RdataType::AAAA,
                5 => RdataType::CNAME,
                15 => RdataType::MX,
                2 => RdataType::NS,
                6 => RdataType::SOA,
                16 => RdataType::TXT,
                _ => panic!("Unsupported RDATA type"),
            }),
            parse_rdata,
        )),
        |(name, rclass, rtype_num, ttl, rtype, rdata)| DNSResourceRecord {
            name,
            rtype,
            rclass,
            ttl,
            rdata,
        },
    )(input)
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = match read(filename) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    match parse_header(&data) {
        Ok((remaining, header)) => {
            println!("Header: {:?}", header);
            let mut remaining_data = remaining;
            for _ in 0..header.qdcount {
                match parse_question(remaining_data) {
                    Ok((rest, question)) => {
                        println!("Question: {:?}", question);
                        remaining_data = rest;
                    }
                    Err(e) => {
                        eprintln!("Error parsing question: {:?}", e);
                        return;
                    }
                }
            }
            for _ in 0..header.ancount {
                match parse_resource_record(remaining_data) {
                    Ok((rest, record)) => {
                        println!("Resource Record: {:?}", record);
                        remaining_data = rest;
                    }
                    Err(e) => {
                        eprintln!("Error parsing resource record: {:?}", e);
                        return;
                    }
                }
            }

        }
        Err(e) => {
            eprintln!("Error parsing header: {:?}", e);
        }
    }
}
