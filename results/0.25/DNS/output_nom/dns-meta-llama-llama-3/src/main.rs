use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map},
    error::{context, Error},
    multi::{many1},
    number::complete::{be_u16, be_u32},
    sequence::{tuple},
    IResult,
};
use std::{
    fs::File,
    io::{Read, stdin},
    path::Path,
};

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
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum QClass {
    IN,
    CS,
    CH,
    HS,
    ANY,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum RCode {
    NOERROR,
    FORMERR,
    SERVFAIL,
    NXDOMAIN,
    NOTIMP,
    REFUSED,
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

impl Header {
    fn parse(input: &[u8]) -> IResult<&[u8], Header> {
        context(
            "header",
            tuple((
                be_u16,
                map(be_u16, |x| (x >> 15) != 0),
                map(be_u16, |x| ((x >> 11) & 0x0f) as u8),
                map(be_u16, |x| (x >> 10) & 0x01 != 0),
                map(be_u16, |x| (x >> 9) & 0x01 != 0),
                map(be_u16, |x| (x >> 8) & 0x01 != 0),
                map(be_u16, |x| (x >> 7) & 0x01 != 0),
                map(be_u16, |x| (x & 0x70) >> 4 as u8),
                map(be_u16, |x| match x & 0x0f {
                    0 => RCode::NOERROR,
                    1 => RCode::FORMERR,
                    2 => RCode::SERVFAIL,
                    3 => RCode::NXDOMAIN,
                    4 => RCode::NOTIMP,
                    5 => RCode::REFUSED,
                    _ => RCode::Other((x & 0x0f) as u8),
                }),
                be_u16,
                be_u16,
                be_u16,
                be_u16,
            )),
        )(input)
        .map(|(input, (id, qr, opcode, aa, tc, rd, ra, z, rcode, qdcount, ancount, nscount, arcount))| {
            (
                input,
                Header {
                    id,
                    qr,
                    opcode,
                    aa,
                    tc,
                    rd,
                    ra,
                    z: z.try_into().unwrap(),
                    rcode,
                    qdcount,
                    ancount,
                    nscount,
                    arcount,
                },
            )
        })
    }
}

#[derive(Debug, PartialEq)]
struct Question {
    qname: String,
    qtype: QType,
    qclass: QClass,
}

impl Question {
    fn parse(input: &[u8]) -> IResult<&[u8], Question> {
        context(
            "question",
            tuple((
                map(take_while_m_n(1, 255, |x| x != 0), |x| String::from_utf8_lossy(x).into_owned()),
                map(be_u16, |x| match x {
                    1 => QType::A,
                    2 => QType::NS,
                    3 => QType::MD,
                    4 => QType::MF,
                    5 => QType::CNAME,
                    6 => QType::SOA,
                    7 => QType::MB,
                    8 => QType::MG,
                    9 => QType::MR,
                    10 => QType::NULL,
                    11 => QType::WKS,
                    12 => QType::PTR,
                    13 => QType::HINFO,
                    14 => QType::MINFO,
                    15 => QType::MX,
                    16 => QType::TXT,
                    255 => QType::ANY,
                    252 => QType::AXFR,
                    _ => QType::Other(x),
                }),
                map(be_u16, |x| match x {
                    1 => QClass::IN,
                    2 => QClass::CS,
                    3 => QClass::CH,
                    4 => QClass::HS,
                    255 => QClass::ANY,
                    _ => QClass::Other(x),
                }),
            )),
        )(input)
        .map(|(input, (qname, qtype, qclass))| (input, Question { qname, qtype, qclass }))
    }
}

#[derive(Debug, PartialEq)]
struct ResourceRecord {
    name: String,
    rtype: QType,
    rclass: QClass,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

impl ResourceRecord {
    fn parse(input: &[u8]) -> IResult<&[u8], ResourceRecord> {
        context(
            "resource record",
            tuple((
                map(take_while_m_n(1, 255, |x| x != 0), |x| String::from_utf8_lossy(x).into_owned()),
                map(be_u16, |x| match x {
                    1 => QType::A,
                    2 => QType::NS,
                    3 => QType::MD,
                    4 => QType::MF,
                    5 => QType::CNAME,
                    6 => QType::SOA,
                    7 => QType::MB,
                    8 => QType::MG,
                    9 => QType::MR,
                    10 => QType::NULL,
                    11 => QType::WKS,
                    12 => QType::PTR,
                    13 => QType::HINFO,
                    14 => QType::MINFO,
                    15 => QType::MX,
                    16 => QType::TXT,
                    255 => QType::ANY,
                    252 => QType::AXFR,
                    _ => QType::Other(x),
                }),
                map(be_u16, |x| match x {
                    1 => QClass::IN,
                    2 => QClass::CS,
                    3 => QClass::CH,
                    4 => QClass::HS,
                    255 => QClass::ANY,
                    _ => QClass::Other(x),
                }),
                be_u32,
                be_u16,
                take,
            )),
        )(input)
        .map(|(input, (name, rtype, rclass, ttl, rdlength, rdata))| {
            (
                input,
                ResourceRecord {
                    name,
                    rtype,
                    rclass,
                    ttl,
                    rdlength,
                    rdata,
                },
            )
        })
    }
}

fn main() {
    let mut file: Box<dyn Read> = match std::env::args().nth(1) {
        Some(path) => Box::new(File::open(Path::new(&path)).unwrap()),
        None => Box::new(stdin()),
    };

    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();

    let header = Header::parse(&buffer).unwrap().1;
    let questions = many1(Question::parse)(&buffer[12..]).unwrap().1;
    let answers = many1(ResourceRecord::parse)(&buffer[12 + questions.len() * 10..]).unwrap().1;
    let authorities = many1(ResourceRecord::parse)(&buffer[12 + questions.len() * 10 + answers.len() * 10..]).unwrap().1;
    let additionals = many1(ResourceRecord::parse)(&buffer[12 + questions.len() * 10 + answers.len() * 10 + authorities.len() * 10..]).unwrap().1;

    println!("{:?}", header);
    println!("{:?}", questions);
    println!("{:?}", answers);
    println!("{:?}", authorities);
    println!("{:?}", additionals);
}