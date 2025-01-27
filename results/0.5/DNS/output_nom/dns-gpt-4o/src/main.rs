use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct DnsHeader {
    id: u16,
    flags: u16,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug)]
struct DnsQuestion {
    qname: Vec<String>,
    qtype: u16,
    qclass: u16,
}

#[derive(Debug)]
struct DnsResourceRecord {
    name: Vec<String>,
    rtype: u16,
    rclass: u16,
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
    additional: Vec<DnsResourceRecord>,
}

fn parse_labels(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    let (mut input, mut labels) = (input, Vec::new());
    loop {
        let (i, length) = be_u8(input)?;
        input = i;
        if length == 0 {
            break;
        }
        let (i, label) = map_res(take(length), |bytes: &[u8]| {
            std::str::from_utf8(bytes).map(String::from)
        })(input)?;
        input = i;
        labels.push(label);
    }
    Ok((input, labels))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, (id, flags, qdcount, ancount, nscount, arcount)) = tuple((
        be_u16, be_u16, be_u16, be_u16, be_u16, be_u16
    ))(input)?;
    Ok((input, DnsHeader { id, flags, qdcount, ancount, nscount, arcount }))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_labels(input)?;
    let (input, (qtype, qclass)) = pair(be_u16, be_u16)(input)?;
    Ok((input, DnsQuestion { qname, qtype, qclass }))
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    let (input, name) = parse_labels(input)?;
    let (input, (rtype, rclass, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((input, DnsResourceRecord {
        name, rtype, rclass, ttl, rdlength, rdata: rdata.to_vec(),
    }))
}

fn parse_dns_packet(input: &[u8]) -> IResult<&[u8], DnsPacket> {
    let (input, header) = parse_header(input)?;
    let (input, questions) = count(parse_question, header.qdcount as usize)(input)?;
    let (input, answers) = count(parse_resource_record, header.ancount as usize)(input)?;
    let (input, authorities) = count(parse_resource_record, header.nscount as usize)(input)?;
    let (input, additional) = count(parse_resource_record, header.arcount as usize)(input)?;
    Ok((input, DnsPacket {
        header, questions, answers, authorities, additional,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_dns_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse DNS packet: {:?}", e),
    }
}