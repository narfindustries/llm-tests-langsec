use nom::{
    bytes::complete::{take},
    combinator::map,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
    error::Error,
};
use std::fs;
use std::path::Path;

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
    rdlength: u16,
    rdata: Vec<u8>,
}

fn parse_domain_name(input: &[u8]) -> IResult<&[u8], String, Error<&[u8]>> {
    let mut domain_name = String::new();
    let mut remaining = input;
    loop {
        let (new_remaining, length) = be_u8(remaining)?;
        if length == 0 {
            break;
        }
        let (new_remaining, label) = take(length as usize)(new_remaining)?;
        domain_name.push_str(std::str::from_utf8(label).unwrap());
        domain_name.push('.');
        remaining = new_remaining;
    }
    Ok((remaining, domain_name.trim_end_matches('.').to_string()))
}

fn be_u8(input: &[u8]) -> IResult<&[u8], u8, Error<&[u8]>> {
    map(take(1usize), |bytes: &[u8]| bytes[0])(input)
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader, Error<&[u8]>> {
    let (remaining, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
    ))(input)?;

    let qr = (flags >> 15) & 0x0001 != 0;
    let opcode = ((flags >> 11) & 0x000F) as u8;
    let aa = (flags >> 10) & 0x0001 != 0;
    let tc = (flags >> 9) & 0x0001 != 0;
    let rd = (flags >> 8) & 0x0001 != 0;
    let ra = (flags >> 7) & 0x0001 != 0;
    let z = ((flags >> 4) & 0x0007) as u8;
    let rcode = (flags & 0x000F) as u8;

    Ok((
        remaining,
        DnsHeader {
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
        },
    ))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion, Error<&[u8]>> {
    let (remaining, (qname, qtype, qclass)) = tuple((parse_domain_name, be_u16, be_u16))(input)?;
    Ok((
        remaining,
        DnsQuestion {
            qname,
            qtype,
            qclass,
        },
    ))
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord, Error<&[u8]>> {
    let (remaining, (name, rtype, rclass, ttl, rdlength, rdata)) = tuple((
        parse_domain_name,
        be_u16,
        be_u16,
        be_u32,
        be_u16,
        |i: &[u8]| take(be_u16(i)?.1 as usize)(i), //Fixed take usage
    ))(input)?;
    Ok((
        remaining,
        DnsResourceRecord {
            name,
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let data = fs::read(path).expect("Failed to read file");

    let result = parse_dns_header(&data);

    match result {
        Ok((remaining, header)) => {
            println!("DNS Header: {:?}", header);

            let mut remaining_data = remaining;
            for _ in 0..header.qdcount {
                let question_result = parse_dns_question(remaining_data);
                match question_result {
                    Ok((rem, question)) => {
                        println!("Question: {:?}", question);
                        remaining_data = rem;
                    }
                    Err(e) => {
                        eprintln!("Error parsing question: {:?}", e);
                        return;
                    }
                }
            }
            for _ in 0..header.ancount {
                let rr_result = parse_dns_resource_record(remaining_data);
                match rr_result {
                    Ok((rem, rr)) => {
                        println!("Resource Record: {:?}", rr);
                        remaining_data = rem;
                    }
                    Err(e) => {
                        eprintln!("Error parsing resource record: {:?}", e);
                        return;
                    }
                }
            }
            for _ in 0..header.nscount {
                let rr_result = parse_dns_resource_record(remaining_data);
                match rr_result {
                    Ok((rem, rr)) => {
                        println!("Authority Record: {:?}", rr);
                        remaining_data = rem;
                    }
                    Err(e) => {
                        eprintln!("Error parsing authority record: {:?}", e);
                        return;
                    }
                }
            }
            for _ in 0..header.arcount {
                let rr_result = parse_dns_resource_record(remaining_data);
                match rr_result {
                    Ok((rem, rr)) => {
                        println!("Additional Record: {:?}", rr);
                        remaining_data = rem;
                    }
                    Err(e) => {
                        eprintln!("Error parsing additional record: {:?}", e);
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