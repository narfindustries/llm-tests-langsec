use nom::{
    bytes::complete::take,
    combinator::map,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs;
use std::str::from_utf8;

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

fn parse_domain_name(input: &[u8]) -> IResult<&[u8], String> {
    let mut domain_name = String::new();
    let mut remaining_input = input;
    loop {
        let (input_after_label, label_len) = take(1usize)(remaining_input)?;
        let label_len = label_len[0] as usize;
        if label_len == 0 {
            break;
        }
        let (input_after_label_data, label_data) = take(label_len)(input_after_label)?;
        let label_str = from_utf8(label_data).unwrap();
        domain_name.push_str(label_str);
        domain_name.push('.');
        remaining_input = input_after_label_data;
    }
    Ok((remaining_input, domain_name.trim_end_matches('.').to_string()))
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    map(
        tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16)),
        |(id, flags, qdcount, ancount, nscount, arcount)| DnsHeader {
            id,
            flags,
            qdcount,
            ancount,
            nscount,
            arcount,
        },
    )(input)
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    map(tuple((parse_domain_name, be_u16, be_u16)), |(qname, qtype, qclass)| DnsQuestion {
        qname,
        qtype,
        qclass,
    })(input)
}

fn parse_dns_resource_record(input: &[u8]) -> IResult<&[u8], DnsResourceRecord> {
    let (input, name) = parse_domain_name(input)?;
    let (input, rtype) = be_u16(input)?;
    let (input, rclass) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength as usize)(input)?;
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

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], (DnsHeader, Vec<DnsQuestion>, Vec<DnsResourceRecord>, Vec<DnsResourceRecord>, Vec<DnsResourceRecord>)> {
    let (input, header) = parse_dns_header(input)?;
    let (input, questions) = nom::multi::count(parse_dns_question, header.qdcount as usize)(input)?;
    let (input, answers) = nom::multi::count(parse_dns_resource_record, header.ancount as usize)(input)?;
    let (input, ns) = nom::multi::count(parse_dns_resource_record, header.nscount as usize)(input)?;
    let (input, ar) = nom::multi::count(parse_dns_resource_record, header.arcount as usize)(input)?;
    Ok((input, (header, questions, answers, ns, ar)))
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
        Ok((_, (header, questions, answers, ns, ar))) => {
            println!("Header: {:?}", header);
            println!("Questions: {:?}", questions);
            println!("Answers: {:?}", answers);
            println!("NS: {:?}", ns);
            println!("AR: {:?}", ar);
        }
        Err(e) => {
            eprintln!("Error parsing DNS message: {:?}", e);
            std::process::exit(1);
        }
    }
}
