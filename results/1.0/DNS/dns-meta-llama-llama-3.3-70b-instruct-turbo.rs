use nom::{
    bytes::complete::{tag, take},
    combinator::{map, many1},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Result},
};

#[derive(Debug, PartialEq)]
enum QueryType {
    A,
    NS,
    MD,
    MF,
    CNAME,
    SOA,
    MX,
    TXT,
    AAAA,
    Other(u16),
}

impl From<u16> for QueryType {
    fn from(t: u16) -> Self {
        match t {
            1 => QueryType::A,
            2 => QueryType::NS,
            3 => QueryType::MD,
            4 => QueryType::MF,
            5 => QueryType::CNAME,
            6 => QueryType::SOA,
            15 => QueryType::MX,
            16 => QueryType::TXT,
            28 => QueryType::AAAA,
            _ => QueryType::Other(t),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Class {
    IN,
    CS,
    CH,
    HS,
    Other(u16),
}

impl From<u16> for Class {
    fn from(c: u16) -> Self {
        match c {
            1 => Class::IN,
            2 => Class::CS,
            3 => Class::CH,
            4 => Class::HS,
            _ => Class::Other(c),
        }
    }
}

#[derive(Debug, PartialEq)]
enum OpCode {
    QUERY,
    IQUERY,
    STATUS,
    Other(u8),
}

impl From<u8> for OpCode {
    fn from(o: u8) -> Self {
        match o {
            0 => OpCode::QUERY,
            1 => OpCode::IQUERY,
            2 => OpCode::STATUS,
            _ => OpCode::Other(o),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ResponseCode {
    NOERROR,
    FORMERR,
    SERVFAIL,
    Other(u8),
}

impl From<u8> for ResponseCode {
    fn from(r: u8) -> Self {
        match r {
            0 => ResponseCode::NOERROR,
            1 => ResponseCode::FORMERR,
            2 => ResponseCode::SERVFAIL,
            _ => ResponseCode::Other(r),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Header {
    id: u16,
    qr: bool,
    opcode: OpCode,
    aa: bool,
    tc: bool,
    rd: bool,
    ra: bool,
    z: u8,
    rcode: ResponseCode,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, id) = be_u16(input)?;
    let (input, qr) = map(take(1usize), |x: &[u8]| x[0] & 0x80 != 0)(input)?;
    let (input, opcode) = map(take(1usize), |x: &[u8]| OpCode::from((x[0] >> 3) & 0x0F))(input)?;
    let (input, aa) = map(take(1usize), |x: &[u8]| x[0] & 0x04 != 0)(input)?;
    let (input, tc) = map(take(1usize), |x: &[u8]| x[0] & 0x02 != 0)(input)?;
    let (input, rd) = map(take(1usize), |x: &[u8]| x[0] & 0x01 != 0)(input)?;
    let (input, ra) = map(take(1usize), |x: &[u8]| x[0] & 0x80 != 0)(input)?;
    let (input, z) = map(take(1usize), |x: &[u8]| x[0] & 0x70)(input)?;
    let (input, rcode) = map(take(1usize), |x: &[u8]| ResponseCode::from(x[0] & 0x0F))(input)?;
    let parts = [id, qr, opcode, aa, tc, rd, ra, z, rcode];
    let header = Header {
        id,
        qr,
        opcode,
        aa,
        tc,
        rd,
        ra,
        z,
        rcode,
    };
    Ok((input, header))
}

fn parse_label(input: &[u8]) -> IResult<&[u8], String> {
    let (input, len) = take(1usize)(input)?;
    let (input, data) = take(len[0] as usize)(input)?;
    let data = String::from_utf8_lossy(data).into_owned();
    Ok((input, data))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], (String, QueryType, Class)> {
    let (input, labels) = many1(parse_label)(input)?;
    let name: String = labels.into_iter().map(|label| label + ".").collect::<String>().trim_end_matches('.').to_string();
    let (input, qtype) = map(be_u16, QueryType::from)(input)?;
    let (input, qclass) = map(be_u16, Class::from)(input)?;
    Ok((input, (name, qtype, qclass)))
}

fn parse_rr(input: &[u8]) -> IResult<&[u8], (String, u16, u32, u16, Vec<u8>)> {
    let (input, labels) = many1(parse_label)(input)?;
    let name: String = labels.into_iter().map(|label| label + ".").collect::<String>().trim_end_matches('.').to_string();
    let (input, rtype) = be_u16(input)?;
    let (input, rclass) = be_u16(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength as usize)(input)?;
    Ok((input, (name, rtype, ttl, rdlength, rdata.to_vec())))
}

fn parse_dns(input: &[u8]) -> IResult<&[u8], (Header, Vec<(String, QueryType, Class)>, Vec<(String, u16, u32, u16, Vec<u8>)>, Vec<(String, u16, u32, u16, Vec<u8>)>)> {
    let (input, header) = parse_header(input)?;
    let (input, questions) = many1(parse_question)(input)?;
    let (input, answers) = many1(parse_rr)(input)?;
    let (input, authorities) = many1(parse_rr)(input)?;
    Ok((input, (header, questions, answers, authorities)))
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    let dns = parse_dns(&buffer).unwrap().1;
    println!("{:?}", dns);
    Ok(())
}