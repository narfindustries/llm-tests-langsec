use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map},
    multi::{many1, many_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug, PartialEq)]
enum QueryType {
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
    RP,
    AFSDB,
    X25,
    ISDN,
    RT,
    NSAP,
    NsapPtr,
    SIG,
    KEY,
    PX,
    GPOS,
    AAAA,
    LOC,
    NXT,
    EID,
    NIMLOC,
    SRV,
    ATMA,
    NAPTR,
    KX,
    CERT,
    A6,
    DNAME,
    SINK,
    OPT,
    APL,
    DS,
    SSHFP,
    IPSECKEY,
    RRSIG,
    NSEC,
    DNSKEY,
    DHCID,
    NSEC3,
    NSEC3PARAM,
    TLSA,
    SMIMEA,
    HIP,
    NINFO,
    RKEY,
    TALINK,
    CDS,
    CDNSKEY,
    OPENPGPKEY,
    CSYNC,
    ZONEMD,
}

impl QueryType {
    fn from_u16(value: u16) -> Result<Self, nom::Err<nom::error::Error<Vec<u8>>>> {
        match value {
            1 => Ok(QueryType::A),
            2 => Ok(QueryType::NS),
            3 => Ok(QueryType::MD),
            4 => Ok(QueryType::MF),
            5 => Ok(QueryType::CNAME),
            6 => Ok(QueryType::SOA),
            7 => Ok(QueryType::MB),
            8 => Ok(QueryType::MG),
            9 => Ok(QueryType::MR),
            10 => Ok(QueryType::NULL),
            11 => Ok(QueryType::WKS),
            12 => Ok(QueryType::PTR),
            13 => Ok(QueryType::HINFO),
            14 => Ok(QueryType::MINFO),
            15 => Ok(QueryType::MX),
            16 => Ok(QueryType::TXT),
            17 => Ok(QueryType::RP),
            18 => Ok(QueryType::AFSDB),
            19 => Ok(QueryType::X25),
            20 => Ok(QueryType::ISDN),
            21 => Ok(QueryType::RT),
            22 => Ok(QueryType::NSAP),
            23 => Ok(QueryType::NsapPtr),
            24 => Ok(QueryType::SIG),
            25 => Ok(QueryType::KEY),
            26 => Ok(QueryType::PX),
            27 => Ok(QueryType::GPOS),
            28 => Ok(QueryType::AAAA),
            29 => Ok(QueryType::LOC),
            30 => Ok(QueryType::NXT),
            31 => Ok(QueryType::EID),
            32 => Ok(QueryType::NIMLOC),
            33 => Ok(QueryType::SRV),
            34 => Ok(QueryType::ATMA),
            35 => Ok(QueryType::NAPTR),
            36 => Ok(QueryType::KX),
            37 => Ok(QueryType::CERT),
            38 => Ok(QueryType::A6),
            39 => Ok(QueryType::DNAME),
            40 => Ok(QueryType::SINK),
            41 => Ok(QueryType::OPT),
            42 => Ok(QueryType::APL),
            43 => Ok(QueryType::DS),
            44 => Ok(QueryType::SSHFP),
            45 => Ok(QueryType::IPSECKEY),
            46 => Ok(QueryType::RRSIG),
            47 => Ok(QueryType::NSEC),
            48 => Ok(QueryType::DNSKEY),
            49 => Ok(QueryType::DHCID),
            50 => Ok(QueryType::NSEC3),
            51 => Ok(QueryType::NSEC3PARAM),
            52 => Ok(QueryType::TLSA),
            53 => Ok(QueryType::SMIMEA),
            55 => Ok(QueryType::HIP),
            56 => Ok(QueryType::NINFO),
            57 => Ok(QueryType::RKEY),
            58 => Ok(QueryType::TALINK),
            59 => Ok(QueryType::CDS),
            60 => Ok(QueryType::CDNSKEY),
            61 => Ok(QueryType::OPENPGPKEY),
            62 => Ok(QueryType::CSYNC),
            63 => Ok(QueryType::ZONEMD),
            _ => Err(nom::Err::Error(nom::error::Error::new(vec![], nom::error::ErrorKind::NonDigit))),
        }
    }
}

#[derive(Debug, PartialEq)]
enum QueryClass {
    IN,
    CS,
    CH,
    HS,
}

impl QueryClass {
    fn from_u16(value: u16) -> Result<Self, nom::Err<nom::error::Error<Vec<u8>>>> {
        match value {
            1 => Ok(QueryClass::IN),
            2 => Ok(QueryClass::CS),
            3 => Ok(QueryClass::CH),
            4 => Ok(QueryClass::HS),
            _ => Err(nom::Err::Error(nom::error::Error::new(vec![], nom::error::ErrorKind::NonDigit))),
        }
    }
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
    rcode: u8,
}

#[derive(Debug, PartialEq)]
struct Question {
    qname: Vec<u8>,
    qtype: QueryType,
    qclass: QueryClass,
}

#[derive(Debug, PartialEq)]
struct Answer {
    name: Vec<u8>,
    type_: QueryType,
    class: QueryClass,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, id) = be_u16(input)?;
    let (input, qr) = map(be_u8, |x| (x & 0x80) != 0)(input)?;
    let (input, opcode) = map(be_u8, |x| x & 0x78 >> 3)(input)?;
    let (input, aa) = map(be_u8, |x| (x & 0x04) != 0)(input)?;
    let (input, tc) = map(be_u8, |x| (x & 0x02) != 0)(input)?;
    let (input, rd) = map(be_u8, |x| (x & 0x01) != 0)(input)?;
    let (input, ra) = map(be_u8, |x| (x & 0x80) != 0)(input)?;
    let (input, z) = map(be_u8, |x| x & 0x70 >> 4)(input)?;
    let (input, rcode) = map(be_u8, |x| x & 0x0f)(input)?;
    Ok((input, Header { id, qr, opcode, aa, tc, rd, ra, z, rcode }))
}

fn parse_question(input: &[u8]) -> IResult<&[u8], Question> {
    let (input, qname) = many_m_n(1, 255, take_while_m_n(1, 63, |x| x != 0))(input)?;
    let (input, qtype_value) = be_u16(input)?;
    let qtype = QueryType::from_u16(qtype_value)?;
    let (input, qclass_value) = be_u16(input)?;
    let qclass = QueryClass::from_u16(qclass_value)?;
    let qname = qname.into_iter().flatten().cloned().collect::<Vec<_>>();
    Ok((input, Question { qname, qtype, qclass }))
}

fn parse_answer(input: &[u8]) -> IResult<&[u8], Answer> {
    let (input, name) = many_m_n(1, 255, take_while_m_n(1, 63, |x| x != 0))(input)?;
    let (input, type_value) = be_u16(input)?;
    let type_ = QueryType::from_u16(type_value)?;
    let (input, class_value) = be_u16(input)?;
    let class = QueryClass::from_u16(class_value)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    let name = name.into_iter().flatten().cloned().collect::<Vec<_>>();
    let rdata = rdata.to_vec();
    Ok((input, Answer { name, type_, class, ttl, rdlength, rdata }))
}

fn parse_dns(input: &[u8]) -> IResult<&[u8], (Header, Vec<Question>, Vec<Answer>)> {
    let (input, header) = parse_header(input)?;
    let (input, questions) = many1(parse_question)(input)?;
    let (input, answers) = many1(parse_answer)(input)?;
    Ok((input, (header, questions, answers)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut input = Vec::new();
    file.read_to_end(&mut input).expect("Failed to read file");
    let result = parse_dns(&input);
    match result {
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