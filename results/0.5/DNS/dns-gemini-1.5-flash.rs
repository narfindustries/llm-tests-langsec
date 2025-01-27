use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, rest},
    number::complete::be_u16,
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::read;
use std::net::IpAddr;
use std::str::FromStr;

#[derive(Debug)]
enum RdataType {
    A(IpAddr),
    NS(Vec<u8>), // Placeholder for more complex types
    CNAME(Vec<u8>), // Placeholder for more complex types
    MX(u16, Vec<u8>), // Placeholder for more complex types
    AAAA(IpAddr),
    // Add other record types as needed
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
    qname: Vec<u8>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DNSResourceRecord {
    name: Vec<u8>,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdata: RdataType,
}


fn parse_name(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut result = Vec::new();
    let mut remaining = input;
    loop {
        let (r, len) = be_u8(remaining)?;
        if len == 0 {
            break;
        }
        let (r2, label) = take(len as usize)(r)?;
        result.extend_from_slice(label);
        result.push(b'.');
        remaining = r2;
    }
    Ok((remaining, result))
}

fn be_u8(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1usize), |x: &[u8]| x[0])(input)
}

fn parse_rdata_a(input: &[u8]) -> IResult<&[u8], RdataType> {
    map_res(take(4usize), |x: &[u8]| {
        IpAddr::from_str(&format!("{}.{}.{}.{}", x[0], x[1], x[2], x[3]))
            .map(RdataType::A)
    })(input)
}

fn parse_rdata_aaaa(input: &[u8]) -> IResult<&[u8], RdataType> {
    map_res(take(16usize), |x: &[u8]| {
        let mut ip_str = String::new();
        for i in 0..8 {
            ip_str.push_str(&format!("{:02x}", x[i * 2]));
            ip_str.push_str(&format!("{:02x}", x[i * 2 + 1]));
            if i < 7 {
                ip_str.push(':');
            }
        }
        IpAddr::from_str(&ip_str).map(RdataType::AAAA)

    })(input)
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], DNSResourceRecord> {
    let (input, name) = parse_name(input)?;
    let (input, (rtype, rclass, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = match rtype {
        1 => map(parse_rdata_a, |x| x)(input)?,
        28 => map(rest, |x| RdataType::NS(x.to_vec()))(input)?,
        5 => map(rest, |x| RdataType::CNAME(x.to_vec()))(input)?,
        15 => map(tuple((be_u16, rest)), |(pref, x)| RdataType::MX(pref, x.to_vec()))(input)?,
        28 => map(parse_rdata_aaaa, |x| x)(input)?,
        _ => unimplemented!(),
    };
    Ok((input, DNSResourceRecord { name, rtype, rclass, ttl, rdata }))
}


fn parse_question(input: &[u8]) -> IResult<&[u8], DNSQuestion> {
    let (input, qname) = parse_name(input)?;
    let (input, (qtype, qclass)) = tuple((be_u16, be_u16))(input)?;
    Ok((input, DNSQuestion { qname, qtype, qclass }))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DNSHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
    Ok((input, DNSHeader { id, flags, qdcount, ancount, nscount, arcount }))
}

fn be_u32(i: &[u8]) -> IResult<&[u8], u32> {
    map(take(4usize), |x: &[u8]| u32::from_be_bytes([x[0], x[1], x[2], x[3]]))(i)
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
            let mut input = remaining;
            for _ in 0..header.qdcount {
                match parse_question(input) {
                    Ok((r, q)) => {
                        println!("Question: {:?}", q);
                        input = r;
                    }
                    Err(e) => {
                        eprintln!("Error parsing question: {:?}", e);
                        return;
                    }
                }
            }
            for _ in 0..header.ancount {
                match parse_resource_record(input) {
                    Ok((r, rr)) => {
                        println!("Resource Record: {:?}", rr);
                        input = r;
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
