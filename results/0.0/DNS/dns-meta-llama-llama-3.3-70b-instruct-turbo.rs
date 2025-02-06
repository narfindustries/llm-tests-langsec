use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, map_res},
    error::{Error, ErrorKind},
    multi::{many0, many1},
    number::complete::{be_u16, be_u32},
    IResult,
};
use nom::error::ParseError;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum QType {
    A,
    NS,
    MD,
    MF,
    CNAME,
    SOA,
    MB,
    MG,
    MR,
    NULL,
    WKS,
    PTR,
    HINFO,
    MINFO,
    MX,
    TXT,
    ANY,
    AXFR,
    MAILB,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum QClass {
    IN,
    CS,
    CH,
    HS,
    NONE,
    ANY,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum RCode {
    NoError,
    FormatError,
    ServerFailure,
    NameError,
    NotImplemented,
    Refused,
    Other(u8),
}

#[derive(Debug, PartialEq)]
struct Header {
    id: u16,
    qr: bool,
    opcode: u8,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: u8,
    rcode: RCode,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

#[derive(Debug, PartialEq)]
struct Question {
    qname: Vec<String>,
    qtype: QType,
    qclass: QClass,
}

#[derive(Debug, PartialEq)]
struct ResourceRecord {
    name: Vec<String>,
    rtype: QType,
    rclass: QClass,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

fn parse_qtype(input: &[u8]) -> IResult<&[u8], QType> {
    map_res(be_u16, |x| match x {
        1 => Ok(QType::A),
        2 => Ok(QType::NS),
        3 => Ok(QType::MD),
        4 => Ok(QType::MF),
        5 => Ok(QType::CNAME),
        6 => Ok(QType::SOA),
        7 => Ok(QType::MB),
        8 => Ok(QType::MG),
        9 => Ok(QType::MR),
        10 => Ok(QType::NULL),
        11 => Ok(QType::WKS),
        12 => Ok(QType::PTR),
        13 => Ok(QType::HINFO),
        14 => Ok(QType::MINFO),
        15 => Ok(QType::MX),
        16 => Ok(QType::TXT),
        17..=252 => Ok(QType::Other(x)),
        253 => Ok(QType::ANY),
        254 => Ok(QType::AXFR),
        255 => Ok(QType::MAILB),
        _ => Err(Error::new(input, ErrorKind::NonEmpty)),
    })(input)
}

fn parse_qclass(input: &[u8]) -> IResult<&[u8], QClass> {
    map_res(be_u16, |x| match x {
        1 => Ok(QClass::IN),
        2 => Ok(QClass::CS),
        3 => Ok(QClass::CH),
        4 => Ok(QClass::HS),
        5..=253 => Ok(QClass::Other(x)),
        254 => Ok(QClass::NONE),
        255 => Ok(QClass::ANY),
        _ => Err(Error::new(input, ErrorKind::NonEmpty)),
    })(input)
}

fn parse_rcode(input: &[u8]) -> IResult<&[u8], RCode> {
    map_res(be_u16, |x| match x {
        0 => Ok(RCode::NoError),
        1 => Ok(RCode::FormatError),
        2 => Ok(RCode::ServerFailure),
        3 => Ok(RCode::NameError),
        4 => Ok(RCode::NotImplemented),
        5 => Ok(RCode::Refused),
        6..=15 => Ok(RCode::Other(x as u8)),
        _ => Err(Error::new(input, ErrorKind::NonEmpty)),
    })(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, id) = be_u16(input)?;
    let (input, flags) = take(2usize)(input)?;
    let qr = (flags[0] & 0x80) != 0;
    let opcode = (flags[0] & 0x78) >> 3;
    let aa = (flags[0] & 0x04) != 0;
    let tc = (flags[0] & 0x02) != 0;
    let rd = (flags[0] & 0x01) != 0;
    let ra = (flags[1] & 0x80) != 0;
    let z = (flags[1] & 0x70) >> 4;
    let rcode = match flags[1] & 0x0f {
        0 => RCode::NoError,
        1 => RCode::FormatError,
        2 => RCode::ServerFailure,
        3 => RCode::NameError,
        4 => RCode::NotImplemented,
        5 => RCode::Refused,
        _ => RCode::Other(flags[1] & 0x0f),
    };
    let (input, qdcount) = be_u16(input)?;
    let (input, ancount) = be_u16(input)?;
    let (input, nscount) = be_u16(input)?;
    let (input, arcount) = be_u16(input)?;
    Ok((
        input,
        Header {
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

fn parse_question(input: &[u8]) -> IResult<&[u8], Question> {
    let (input, qname) = many1(take_while_m_n(1, 63, |x| x != 0))(input)?;
    let qname: Vec<String> = qname
        .into_iter()
        .map(|x| String::from_utf8_lossy(x).into_owned())
        .collect();
    let (input, qtype) = parse_qtype(input)?;
    let (input, qclass) = parse_qclass(input)?;
    Ok((
        input,
        Question {
            qname,
            qtype,
            qclass,
        },
    ))
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], ResourceRecord> {
    let (input, name) = many1(take_while_m_n(1, 63, |x| x != 0))(input)?;
    let name: Vec<String> = name
        .into_iter()
        .map(|x| String::from_utf8_lossy(x).into_owned())
        .collect();
    let (input, rtype) = parse_qtype(input)?;
    let (input, rclass) = parse_qclass(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength as usize)(input)?;
    Ok((
        input,
        ResourceRecord {
            name,
            rtype,
            rclass,
            ttl,
            rdlength,
            rdata: rdata.to_vec(),
        },
    ))
}

fn parse_dns(input: &[u8]) -> IResult<&[u8], (Header, Vec<Question>, Vec<ResourceRecord>)> {
    let (input, header) = parse_header(input)?;
    let (input, questions) = many0(parse_question)(input)?;
    let (input, answers) = many0(parse_resource_record)(input)?;
    Ok((input, (header, questions, answers)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();
    match parse_dns(&data) {
        Ok((_, (header, questions, answers))) => {
            println!("Header: {:?}", header);
            println!("Questions: {:?}", questions);
            println!("Answers: {:?}", answers);
        }
        Err(err) => {
            println!("Error: {:?}", err);
        }
    }
}