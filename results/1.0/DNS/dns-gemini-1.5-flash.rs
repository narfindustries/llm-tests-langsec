use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, peek},
    number::streaming::{be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::net::IpAddr;
use std::str::from_utf8;

#[derive(Debug, PartialEq)]
enum RdataType {
    A,
    AAAA,
    CNAME,
    MX,
    NS,
    SOA,
    TXT,
    // Add other record types as needed
}

#[derive(Debug, PartialEq)]
struct Rdata {
    rtype: RdataType,
    data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct ResourceRecord {
    name: String,
    rtype: RdataType,
    class: u16,
    ttl: u32,
    rdata: Rdata,
}

#[derive(Debug, PartialEq)]
struct DNSHeader {
    id: u16,
    flags: u16,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
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
    Ok((remaining, name.trim_end_matches(".").to_string()))
}

fn parse_rdata(rtype: RdataType, input: &[u8]) -> IResult<&[u8], Rdata> {
    match rtype {
        RdataType::A => {
            map(take(4usize), |data: &[u8]| Rdata {
                rtype: RdataType::A,
                data: data.to_vec(),
            })(input)
        }
        RdataType::AAAA => {
            map(take(16usize), |data: &[u8]| Rdata {
                rtype: RdataType::AAAA,
                data: data.to_vec(),
            })(input)
        }
        RdataType::CNAME | RdataType::NS | RdataType::MX => {
            map(parse_name, |name: String| Rdata {
                rtype,
                data: name.as_bytes().to_vec(),
            })(input)
        }
        _ => {
            // Handle other record types here
            map(take(0usize), |_| Rdata { rtype, data: vec![] })(input)
        }
    }
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], ResourceRecord> {
    let (rest, (name, rtype, class, ttl, rdata)) = tuple((
        parse_name,
        map(be_u16, |x| match x {
            1 => RdataType::A,
            28 => RdataType::AAAA,
            5 => RdataType::CNAME,
            15 => RdataType::MX,
            2 => RdataType::NS,
            6 => RdataType::SOA,
            16 => RdataType::TXT,
            _ => panic!("Unsupported RdataType"),
        }),
        be_u16,
        be_u32,
        map(parse_rdata, |rdata| rdata),
    ))(input)?;
    Ok((
        rest,
        ResourceRecord {
            name,
            rtype,
            class,
            ttl,
            rdata,
        },
    ))
}


fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (rest, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
    ))(input)?;
    Ok((
        rest,
        DNSHeader {
            id,
            flags,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dns_header(&buffer) {
        Ok((rest, header)) => {
            println!("DNS Header: {:?}", header);
            //Further parsing for Resource Records would go here based on counts from header
        }
        Err(e) => {
            eprintln!("Failed to parse DNS header: {}", e);
        }
    }
}
