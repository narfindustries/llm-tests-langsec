use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, map_res, opt},
    error::{context, ErrorKind},
    multi::{length_data, take_while_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{tuple, delimited},
    IResult, Parser,
};
use std::{
    fs::File,
    io::{Read, BufReader},
    str,
};

#[derive(Debug)]
enum DnsClass {
    Internet,
    CSNet,
    Chaos,
    Hesiod,
    None,
    Any,
    Other(u16),
}

#[derive(Debug)]
enum DnsType {
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
    NSAP_PTR,
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
    Other(u16),
}

#[derive(Debug)]
struct DnsQuestion {
    name: Vec<String>,
    dns_type: DnsType,
    dns_class: DnsClass,
}

#[derive(Debug)]
enum DnsRdata {
    A(String),
    NS(String),
    CNAME(String),
    SOA {
        mname: String,
        rname: String,
        serial: u32,
        refresh: u32,
        retry: u32,
        expire: u32,
        minimum: u32,
    },
    PTR(String),
    MX {
        preference: u16,
        exchange: String,
    },
    TXT(Vec<String>),
    Other(u16, Vec<u8>),
}

#[derive(Debug)]
struct DnsAnswer {
    name: String,
    dns_type: DnsType,
    dns_class: DnsClass,
    ttl: u32,
    rdata: DnsRdata,
}

fn parse_dotted_string(input: &[u8]) -> IResult<&[u8], String> {
    context(
        "dotted string",
        take_while_m_n(1, 63, |c| (c as char).is_alphanumeric() || (c == b'-')),
    )(input)
    .map(|(input, s)| (input, String::from(str::from_utf8(s).unwrap())))
}

fn parse_domain_name(input: &[u8]) -> IResult<&[u8], Vec<String>> {
    context(
        "domain name",
        map(
            take_while_m_n(1, 255, |c| (c as char).is_alphanumeric() || (c == b'-') || (c == b'.')),
            |s: &[u8]| {
                str::from_utf8(s)
                    .unwrap()
                    .split('.')
                    .map(|s| String::from(s))
                    .collect()
            },
        ),
    )(input)
}

fn parse_dns_class(input: &[u8]) -> IResult<&[u8], DnsClass> {
    context(
        "dns class",
        map_res(be_u16, |class: u16| {
            match class {
                1 => Ok(DnsClass::Internet),
                2 => Ok(DnsClass::CSNet),
                3 => Ok(DnsClass::Chaos),
                4 => Ok(DnsClass::Hesiod),
                254 => Ok(DnsClass::None),
                255 => Ok(DnsClass::Any),
                _ => Ok(DnsClass::Other(class)),
            }
        }),
    )(input)
}

fn parse_dns_type(input: &[u8]) -> IResult<&[u8], DnsType> {
    context(
        "dns type",
        map_res(be_u16, |type_: u16| {
            match type_ {
                1 => Ok(DnsType::A),
                2 => Ok(DnsType::NS),
                3 => Ok(DnsType::MD),
                4 => Ok(DnsType::MF),
                5 => Ok(DnsType::CNAME),
                6 => Ok(DnsType::SOA),
                7 => Ok(DnsType::MB),
                8 => Ok(DnsType::MG),
                9 => Ok(DnsType::MR),
                10 => Ok(DnsType::NULL),
                11 => Ok(DnsType::WKS),
                12 => Ok(DnsType::PTR),
                13 => Ok(DnsType::HINFO),
                14 => Ok(DnsType::MINFO),
                15 => Ok(DnsType::MX),
                16 => Ok(DnsType::TXT),
                17 => Ok(DnsType::RP),
                18 => Ok(DnsType::AFSDB),
                19 => Ok(DnsType::X25),
                20 => Ok(DnsType::ISDN),
                21 => Ok(DnsType::RT),
                22 => Ok(DnsType::NSAP),
                23 => Ok(DnsType::NSAP_PTR),
                24 => Ok(DnsType::SIG),
                25 => Ok(DnsType::KEY),
                26 => Ok(DnsType::PX),
                27 => Ok(DnsType::GPOS),
                28 => Ok(DnsType::AAAA),
                29 => Ok(DnsType::LOC),
                30 => Ok(DnsType::NXT),
                31 => Ok(DnsType::EID),
                32 => Ok(DnsType::NIMLOC),
                33 => Ok(DnsType::SRV),
                34 => Ok(DnsType::ATMA),
                35 => Ok(DnsType::NAPTR),
                36 => Ok(DnsType::KX),
                37 => Ok(DnsType::CERT),
                38 => Ok(DnsType::A6),
                39 => Ok(DnsType::DNAME),
                40 => Ok(DnsType::SINK),
                41 => Ok(DnsType::OPT),
                42 => Ok(DnsType::APL),
                43 => Ok(DnsType::DS),
                44 => Ok(DnsType::SSHFP),
                45 => Ok(DnsType::IPSECKEY),
                46 => Ok(DnsType::RRSIG),
                47 => Ok(DnsType::NSEC),
                48 => Ok(DnsType::DNSKEY),
                49 => Ok(DnsType::DHCID),
                50 => Ok(DnsType::NSEC3),
                51 => Ok(DnsType::NSEC3PARAM),
                52 => Ok(DnsType::TLSA),
                53 => Ok(DnsType::SMIMEA),
                _ => Ok(DnsType::Other(type_)),
            }
        }),
    )(input)
}

fn parse_dns_rdata(input: &[u8], type_: DnsType) -> IResult<&[u8], DnsRdata> {
    context(
        "dns rdata",
        match type_ {
            DnsType::A => map(take(4usize), |a: &[u8]| {
                DnsRdata::A(format!("{:?}", a))
            }),
            DnsType::NS => map(parse_domain_name, |ns: Vec<String>| {
                DnsRdata::NS(ns.join("."))
            }),
            DnsType::CNAME => map(parse_domain_name, |cname: Vec<String>| {
                DnsRdata::CNAME(cname.join("."))
            }),
            DnsType::SOA => map(
                tuple((parse_domain_name, parse_domain_name, be_u32, be_u32, be_u32, be_u32, be_u32)),
                |(mname, rname, serial, refresh, retry, expire, minimum): (Vec<String>, Vec<String>, u32, u32, u32, u32, u32)| {
                    DnsRdata::SOA {
                        mname: mname.join("."),
                        rname: rname.join("."),
                        serial,
                        refresh,
                        retry,
                        expire,
                        minimum,
                    }
                },
            ),
            DnsType::PTR => map(parse_domain_name, |ptr: Vec<String>| {
                DnsRdata::PTR(ptr.join("."))
            }),
            DnsType::MX => map(
                tuple((be_u16, parse_domain_name)),
                |(preference, exchange): (u16, Vec<String>)| {
                    DnsRdata::MX {
                        preference,
                        exchange: exchange.join("."),
                    }
                },
            ),
            DnsType::TXT => map(
                take_while_m_n(1, 255, |c| (c as char).is_alphanumeric() || (c == b' ') || (c == b'.')),
                |txt: &[u8]| {
                    DnsRdata::TXT(vec![String::from(str::from_utf8(txt).unwrap())])
                },
            ),
            _ => map(take(1), |_: &[u8]| DnsRdata::Other(type_ as u16, input.to_vec())),
        },
    )(input)
}

fn parse_dns_answer(input: &[u8]) -> IResult<&[u8], DnsAnswer> {
    context(
        "dns answer",
        map(
            tuple((
                parse_domain_name,
                parse_dns_type,
                parse_dns_class,
                be_u32,
                length_data(be_u16),
            )),
            |(name, dns_type, dns_class, ttl, rdata): (Vec<String>, DnsType, DnsClass, u32, Vec<u8>)| {
                DnsAnswer {
                    name: name.join("."),
                    dns_type,
                    dns_class,
                    ttl,
                    rdata: parse_dns_rdata(&rdata, dns_type).unwrap().1,
                }
            },
        ),
    )(input)
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    context(
        "dns question",
        map(
            tuple((parse_domain_name, parse_dns_type, parse_dns_class)),
            |(name, dns_type, dns_class): (Vec<String>, DnsType, DnsClass)| DnsQuestion {
                name: name,
                dns_type,
                dns_class,
            },
        ),
    )(input)
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], (u16, u16, u16, u16, u16, u16)> {
    context(
        "dns header",
        map(
            tuple((be_u16, be_u16, be_u16, be_u16, be_u16, be_u16)),
            |(id, flags, qdcount, ancount, nscount, arcount): (u16, u16, u16, u16, u16, u16)| {
                (id, flags, qdcount, ancount, nscount, arcount)
            },
        ),
    )(input)
}

fn parse_dns(input: &[u8]) -> IResult<&[u8], (u16, u16, u16, u16, u16, u16, Vec<DnsQuestion>, Vec<DnsAnswer>)> {
    context(
        "dns",
        map(
            tuple((
                parse_dns_header,
                take_while_m_n(0, 0, |c| true),
                many_n(0, parse_dns_question, ErrorKind::NonEmpty),
                many_n(0, parse_dns_answer, ErrorKind::NonEmpty),
            )),
            |(header, _, questions, answers): ((u16, u16, u16, u16, u16, u16), &[u8], Vec<DnsQuestion>, Vec<DnsAnswer>)| {
                (header.0, header.1, header.2, header.3, header.4, header.5, questions, answers)
            },
        ),
    )(input)
}

fn main() -> std::io::Result<()> {
    let file = std::env::args().nth(1).expect("Please provide a file path");
    let mut file = File::open(file)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    let dns = parse_dns(&buffer).unwrap().1;

    println!("{:?}", dns);

    Ok(())
}