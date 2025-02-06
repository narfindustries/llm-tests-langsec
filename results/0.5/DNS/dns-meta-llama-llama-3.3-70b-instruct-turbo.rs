use nom::{
    bytes::complete::{take},
    combinator::{map},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
enum DnsClass {
    In,
    Cs,
    Ch,
    Hs,
    Other(u16),
}

#[derive(Debug, Clone)]
enum DnsType {
    A,
    Ns,
    Md,
    Mf,
    Cname,
    Soa,
    Ptr,
    Mx,
    Other(u16),
}

#[derive(Debug, Clone)]
enum DnsOpcode {
    Query,
    InverseQuery,
    Status,
    Other(u8),
}

#[derive(Debug, Clone)]
enum DnsRcode {
    NoError,
    FormatError,
    ServerFailure,
    NameError,
    NotImplemented,
    Refused,
    Other(u8),
}

fn parse_dns_class(input: &[u8]) -> IResult<&[u8], DnsClass> {
    map(be_u16, |class: u16| match class {
        1 => DnsClass::In,
        2 => DnsClass::Cs,
        3 => DnsClass::Ch,
        4 => DnsClass::Hs,
        _ => DnsClass::Other(class),
    })(input)
}

fn parse_dns_type(input: &[u8]) -> IResult<&[u8], DnsType> {
    map(be_u16, |type_: u16| match type_ {
        1 => DnsType::A,
        2 => DnsType::Ns,
        3 => DnsType::Md,
        4 => DnsType::Mf,
        5 => DnsType::Cname,
        6 => DnsType::Soa,
        12 => DnsType::Ptr,
        15 => DnsType::Mx,
        _ => DnsType::Other(type_),
    })(input)
}

fn parse_dns_opcode(input: &[u8]) -> IResult<&[u8], DnsOpcode> {
    map(be_u8, |opcode: u8| match opcode {
        0 => DnsOpcode::Query,
        1 => DnsOpcode::InverseQuery,
        2 => DnsOpcode::Status,
        _ => DnsOpcode::Other(opcode),
    })(input)
}

fn parse_dns_rcode(input: &[u8]) -> IResult<&[u8], DnsRcode> {
    map(be_u8, |rcode: u8| match rcode {
        0 => DnsRcode::NoError,
        1 => DnsRcode::FormatError,
        2 => DnsRcode::ServerFailure,
        3 => DnsRcode::NameError,
        4 => DnsRcode::NotImplemented,
        5 => DnsRcode::Refused,
        _ => DnsRcode::Other(rcode),
    })(input)
}

fn parse_domain_name(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut output = Vec::new();
    let mut input = input;
    loop {
        let (i, label_len) = be_u8(input)?;
        input = i;
        if label_len == 0 {
            break;
        }
        let (i, label) = take(label_len as usize)(input)?;
        input = i;
        output.extend_from_slice(label);
        output.push(b'.');
    }
    Ok((input, output))
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], (u16, bool, DnsOpcode, bool, bool, bool, bool, DnsRcode)> {
    let (input, id) = be_u16(input)?;
    let (input, flags) = be_u16(input)?;
    let qr = (flags >> 15) & 1 != 0;
    let flags_bytes = flags.to_be_bytes();
    let opcode = match flags_bytes[1] {
        0 => DnsOpcode::Query,
        1 => DnsOpcode::InverseQuery,
        2 => DnsOpcode::Status,
        _ => DnsOpcode::Other(flags_bytes[1]),
    };
    let aa = (flags >> 10) & 1 != 0;
    let tc = (flags >> 9) & 1 != 0;
    let rd = (flags >> 8) & 1 != 0;
    let ra = (flags >> 7) & 1 != 0;
    let rcode = match flags_bytes[0] {
        0 => DnsRcode::NoError,
        1 => DnsRcode::FormatError,
        2 => DnsRcode::ServerFailure,
        3 => DnsRcode::NameError,
        4 => DnsRcode::NotImplemented,
        5 => DnsRcode::Refused,
        _ => DnsRcode::Other(flags_bytes[0]),
    };
    Ok((input, (id, qr, opcode, aa, tc, rd, ra, rcode)))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], (Vec<u8>, DnsType, DnsClass)> {
    let (input, name) = parse_domain_name(input)?;
    let (input, type_) = parse_dns_type(input)?;
    let (input, class) = parse_dns_class(input)?;
    Ok((input, (name, type_, class)))
}

fn parse_dns_resource(input: &[u8]) -> IResult<&[u8], (Vec<u8>, DnsType, DnsClass, u32, Vec<u8>)> {
    let (input, name) = parse_domain_name(input)?;
    let (input, type_) = parse_dns_type(input)?;
    let (input, class) = parse_dns_class(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength as usize)(input)?;
    Ok((input, (name, type_, class, ttl, rdata.to_vec())))
}

fn parse_dns_message(input: &[u8]) -> IResult<&[u8], (u16, bool, DnsOpcode, bool, bool, bool, bool, DnsRcode, Vec<(Vec<u8>, DnsType, DnsClass)>, Vec<(Vec<u8>, DnsType, DnsClass, u32, Vec<u8>)>, Vec<(Vec<u8>, DnsType, DnsClass, u32, Vec<u8>)>)> {
    let (input, header) = parse_dns_header(input)?;
    let mut questions = Vec::new();
    let mut input = input;
    for _ in 0..header.0 {
        let (i, q) = parse_dns_question(input)?;
        input = i;
        questions.push(q);
    }
    let mut answers = Vec::new();
    for _ in 0..header.0 {
        let (i, a) = parse_dns_resource(input)?;
        input = i;
        answers.push(a);
    }
    let mut additionals = Vec::new();
    for _ in 0..header.0 {
        let (i, a) = parse_dns_resource(input)?;
        input = i;
        additionals.push(a);
    }
    Ok((input, (header.0, header.1, header.2, header.3, header.4, header.5, header.6, header.7, questions, answers, additionals)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut input = Vec::new();
    file.read_to_end(&mut input).expect("Failed to read file");
    let result = parse_dns_message(&input);
    match result {
        Ok((_, message)) => println!("{:?}", message),
        Err(err) => println!("Error: {:?}", err),
    }
}