use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

// DNS Header
#[derive(Debug)]
struct DnsHeader {
    id: u16,
    flags: u16,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

// DNS Question
#[derive(Debug)]
struct DnsQuestion {
    qname: Vec<String>,
    qtype: u16,
    qclass: u16,
}

// DNS Resource Record
#[derive(Debug)]
struct DnsResourceRecord {
    name: Vec<String>,
    rtype: u16,
    rclass: u16,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

// Parse DNS Header
fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) =
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16))(input)?;
    Ok((
        input,
        DnsHeader {
            id,
            flags,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    ))
}

// Parse DNS Labels (e.g., domain names)
fn parse_labels(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let mut input = input;
    let mut labels = Vec::new();
    loop {
        let (i, length) = be_u8(input)?;
        if length == 0 {
            return Ok((i, labels));
        }
        let (i, label) = map_res(take(length), |bytes: &[u8]| {
            std::str::from_utf8(bytes).map(String::from)
        })(i)?;
        labels.push(label);
        input = i;
    }
}

// Parse DNS Question
fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_labels(input)?;
    let (input, qtype) = be_u16(input)?;
    let (input, qclass) = be_u16(input)?;
    Ok((
        input,
        DnsQuestion {
            qname,
            qtype,
            qclass,
        },
    ))
}

// Parse DNS Resource Record
fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    let (input, name) = parse_labels(input)?;
    let (input, rtype) = be_u16(input)?;
    let (input, rclass) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((
        input,
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

// Parse a DNS packet
fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], (DnsHeader, Vec<DnsQuestion>, Vec<DnsResourceRecord>, Vec<DnsResourceRecord>, Vec<DnsResourceRecord>)> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = count(parse_dns_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_dns_resource_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_dns_resource_record, header.nscount as usize)(input)?;
    let (input, additionals) = count(parse_dns_resource_record, header.arcount as usize)(input)?;
    Ok((input, (header, questions, answers, authorities, additionals)))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dns-binary-file>", args[0]);
        return Ok(());
    }

    // Read the binary DNS file
    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    // Parse the DNS packet
    match parse_dns_packet(&buffer) {
        Ok((_, dns_packet)) => {
            println!("{:#?}", dns_packet);
        }
        Err(e) => {
            eprintln!("Failed to parse DNS packet: {:?}", e);
        }
    }

    Ok(())
}