use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt, map_res, opt, verify},
    error::{Error, ErrorKind},
    multi::{length_data, take_while_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::{
    collections::HashMap,
    fs::File,
    io::{self, Read},
    str,
};

#[derive(Debug, PartialEq)]
enum DnsType {
    A,
    NS,
    CNAME,
    SOA,
    PTR,
    MX,
    TXT,
    AAAA,
    SRV,
    Other(u16),
}

impl DnsType {
    fn from_u16(value: u16) -> Self {
        match value {
            1 => DnsType::A,
            2 => DnsType::NS,
            5 => DnsType::CNAME,
            6 => DnsType::SOA,
            12 => DnsType::PTR,
            15 => DnsType::MX,
            16 => DnsType::TXT,
            28 => DnsType::AAAA,
            33 => DnsType::SRV,
            _ => DnsType::Other(value),
        }
    }
}

#[derive(Debug, PartialEq)]
enum DnsClass {
    IN,
    CS,
    CH,
    HS,
    Other(u16),
}

impl DnsClass {
    fn from_u16(value: u16) -> Self {
        match value {
            1 => DnsClass::IN,
            2 => DnsClass::CS,
            3 => DnsClass::CH,
            4 => DnsClass::HS,
            _ => DnsClass::Other(value),
        }
    }
}

#[derive(Debug, Default, PartialEq)]
struct DnsMessage {
    id: u16,
    flags: u16,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
    questions: Vec<DnsQuestion>,
    answers: Vec<DnsAnswer>,
    nameservers: Vec<DnsAnswer>,
    additional_records: Vec<DnsAnswer>,
}

#[derive(Debug, Default, PartialEq)]
struct DnsQuestion {
    qname: String,
    qtype: DnsType,
    qclass: DnsClass,
}

#[derive(Debug, Default, PartialEq)]
struct DnsAnswer {
    name: String,
    type_: DnsType,
    class: DnsClass,
    ttl: u32,
    rdlength: u16,
    rdata: String,
}

fn parse_dns(input: &[u8]) -> IResult<&[u8], DnsMessage> {
    let (input, id) = be_u16(input)?;
    let (input, flags) = be_u16(input)?;
    let (input, qdcount) = be_u16(input)?;
    let (input, ancount) = be_u16(input)?;
    let (input, nscount) = be_u16(input)?;
    let (input, arcount) = be_u16(input)?;

    let (input, questions) = map(
        take_while_m_n(qdcount as usize, qdcount as usize, |i| parse_question(i).is_ok()),
        |input| input.into_iter().map(|i| parse_question(i).unwrap().1).collect(),
    )(input)?;

    let (input, answers) = map(
        take_while_m_n(ancount as usize, ancount as usize, |i| parse_answer(i).is_ok()),
        |input| input.into_iter().map(|i| parse_answer(i).unwrap().1).collect(),
    )(input)?;

    let (input, nameservers) = map(
        take_while_m_n(nscount as usize, nscount as usize, |i| parse_answer(i).is_ok()),
        |input| input.into_iter().map(|i| parse_answer(i).unwrap().1).collect(),
    )(input)?;

    let (input, additional_records) = map(
        take_while_m_n(arcount as usize, arcount as usize, |i| parse_answer(i).is_ok()),
        |input| input.into_iter().map(|i| parse_answer(i).unwrap().1).collect(),
    )(input)?;

    let dns_message = DnsMessage {
        id,
        flags,
        qdcount,
        ancount,
        nscount,
        arcount,
        questions,
        answers,
        nameservers,
        additional_records,
    };

    Ok((input, dns_message))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, qname) = parse_domain_name(input)?;
    let (input, qtype) = map(be_u16, DnsType::from_u16)(input)?;
    let (input, qclass) = map(be_u16, DnsClass::from_u16)(input)?;

    let dns_question = DnsQuestion {
        qname,
        qtype,
        qclass,
    };

    Ok((input, dns_question))
}

fn parse_answer(input: &[u8]) -> IResult<&[u8], DnsAnswer> {
    let (input, name) = parse_domain_name(input)?;
    let (input, type_) = map(be_u16, DnsType::from_u16)(input)?;
    let (input, class) = map(be_u16, DnsClass::from_u16)(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength as usize)(input)?;

    let rdata_str = str::from_utf8(rdata).unwrap_or("");

    let dns_answer = DnsAnswer {
        name,
        type_,
        class,
        ttl,
        rdlength,
        rdata: rdata_str.to_string(),
    };

    Ok((input, dns_answer))
}

fn parse_domain_name(input: &[u8]) -> IResult<&[u8], String> {
    let mut domain_name = Vec::new();

    let mut input = input;

    loop {
        let (i, len) = be_u8(input)?;
        input = i;

        if len == 0 {
            break;
        }

        let (i, label) = take(len as usize)(input)?;
        input = i;

        domain_name.push(str::from_utf8(&label).unwrap_or(""));
    }

    let domain_name = domain_name.join(".");

    Ok((input, domain_name))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let result = parse_dns(&buffer);

    match result {
        Ok((_, dns_message)) => {
            println!("DNS Message:");
            println!("  ID: {}", dns_message.id);
            println!("  Flags: {}", dns_message.flags);
            println!("  QDCount: {}", dns_message.qdcount);
            println!("  ANCount: {}", dns_message.ancount);
            println!("  NSCount: {}", dns_message.nscount);
            println!("  ARCount: {}", dns_message.arcount);

            for (i, question) in dns_message.questions.iter().enumerate() {
                println!("Question #{}:", i + 1);
                println!("  QName: {}", question.qname);
                println!("  QType: {:?}", question.qtype);
                println!("  QClass: {:?}", question.qclass);
            }

            for (i, answer) in dns_message.answers.iter().enumerate() {
                println!("Answer #{}:", i + 1);
                println!("  Name: {}", answer.name);
                println!("  Type: {:?}", answer.type_);
                println!("  Class: {:?}", answer.class);
                println!("  TTL: {}", answer.ttl);
                println!("  RDLength: {}", answer.rdlength);
                println!("  RData: {}", answer.rdata);
            }

            for (i, nameserver) in dns_message.nameservers.iter().enumerate() {
                println!("Nameserver #{}:", i + 1);
                println!("  Name: {}", nameserver.name);
                println!("  Type: {:?}", nameserver.type_);
                println!("  Class: {:?}", nameserver.class);
                println!("  TTL: {}", nameserver.ttl);
                println!("  RDLength: {}", nameserver.rdlength);
                println!("  RData: {}", nameserver.rdata);
            }

            for (i, additional_record) in dns_message.additional_records.iter().enumerate() {
                println!("Additional Record #{}:", i + 1);
                println!("  Name: {}", additional_record.name);
                println!("  Type: {:?}", additional_record.type_);
                println!("  Class: {:?}", additional_record.class);
                println!("  TTL: {}", additional_record.ttl);
                println!("  RDLength: {}", additional_record.rdlength);
                println!("  RData: {}", additional_record.rdata);
            }
        }
        Err(err) => {
            eprintln!("Error parsing DNS message: {}", err);
        }
    }

    Ok(())
}