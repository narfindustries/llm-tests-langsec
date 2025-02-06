use nom::{
    bytes::complete::{take},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::read;
use std::net::{Ipv4Addr, Ipv6Addr};
use std::str::from_utf8;
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
enum DnsResourceRecordData {
    A(Ipv4Addr),
    AAAA(Ipv6Addr),
    // Add other record types as needed...
    Unknown(Vec<u8>),
}

#[derive(Debug, PartialEq)]
struct DnsResourceRecord {
    name: String,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdlength: u16,
    rdata: DnsResourceRecordData,
}

fn parse_domain_name(i: &[u8]) -> IResult<&[u8], String> {
    let mut domain_name = String::new();
    let mut remaining = i;
    loop {
        let (r, len) = take(1usize)(remaining)?;
        let len = len[0];
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

fn parse_dns_header(i: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (remaining, id) = be_u16(i)?;
    let (remaining, flags) = be_u16(remaining)?;
    let qr = (flags >> 15) & 1 != 0;
    let opcode = (flags >> 11) & 0xf;
    let aa = (flags >> 10) & 1 != 0;
    let tc = (flags >> 9) & 1 != 0;
    let rd = (flags >> 8) & 1 != 0;
    let ra = (flags >> 7) & 1 != 0;
    let rcode = flags & 0xf;
    let (remaining, qdcount) = be_u16(remaining)?;
    let (remaining, ancount) = be_u16(remaining)?;
    let (remaining, nscount) = be_u16(remaining)?;
    let (remaining, arcount) = be_u16(remaining)?;

    Ok((
        remaining,
        DnsHeader {
            id,
            qr,
            opcode: opcode as u8,
            aa,
            tc,
            rd,
            ra,
            rcode: rcode as u8,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

fn parse_dns_question(i: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (remaining, qname) = parse_domain_name(i)?;
    let (remaining, qtype) = be_u16(remaining)?;
    let (remaining, qclass) = be_u16(remaining)?;
    Ok((
        remaining,
        DnsQuestion {
            qname,
            qtype,
            qclass,
        },
    ))
}

fn parse_dns_resource_record_data(rtype: u16, i: &[u8], rdlength: u16) -> IResult<&[u8], DnsResourceRecordData> {
    match rtype {
        1 => {
            let (rem, addr) = tuple((be_u32, be_u32, be_u32, be_u32))(i)?;
            let ip = Ipv4Addr::new(addr.0 as u8, addr.1 as u8, addr.2 as u8, addr.3 as u8);
            Ok((rem, DnsResourceRecordData::A(ip)))
        },
        28 => {
            let (rem, bytes) = take(16usize)(i)?;
            let ip = Ipv6Addr::from(bytes.try_into().unwrap());
            Ok((rem, DnsResourceRecordData::AAAA(ip)))
        },
        _ => Ok((i, DnsResourceRecordData::Unknown(i[..rdlength as usize].to_vec()))),
    }
}


fn parse_dns_resource_record(i: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    let (remaining, name) = parse_domain_name(i)?;
    let (remaining, rtype) = be_u16(remaining)?;
    let (remaining, rclass) = be_u16(remaining)?;
    let (remaining, ttl) = be_u32(remaining)?;
    let (remaining, rdlength) = be_u16(remaining)?;
    let (remaining, rdata) = parse_dns_resource_record_data(rtype, remaining, rdlength)?;
    Ok((
        remaining,
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
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = read(filename).expect("Failed to read file");

    let result = parse_dns_header(&data);

    match result {
        Ok((remaining, header)) => {
            println!("DNS Header: {:?}", header);

            let mut remaining_data = remaining;
            for _ in 0..header.qdcount {
                let (rem, question) = parse_dns_question(remaining_data).unwrap();
                println!("Question: {:?}", question);
                remaining_data = rem;
            }
            for _ in 0..header.ancount {
                let (rem, rr) = parse_dns_resource_record(remaining_data).unwrap();
                println!("Resource Record: {:?}", rr);
                remaining_data = rem;
            }
            //Process nscount and arcount similarly.
        }
        Err(e) => {
            eprintln!("Error parsing DNS header: {:?}", e);
        }
    }
}
