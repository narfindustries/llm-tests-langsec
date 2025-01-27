use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{flat_map, map, map_opt, verify},
    error::{Error, ErrorKind},
    multi::{length_data, many0, many1, take_while_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{pair, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    str,
};

#[derive(Debug, PartialEq)]
enum DnsClass {
    Internet,
    Csnet,
    Chaos,
    Hesiod,
    None,
    Any,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum DnsType {
    A,
    Ns,
    Md,
    Mf,
    Cname,
    Soa,
    Mb,
    Mg,
    Mr,
    Null,
    Wks,
    Ptr,
    Hinfo,
    Minfo,
    Mx,
    Txt,
    Rp,
    Afsdb,
    X25,
    Isdn,
    Rt,
    Nsap,
    NsapPtr,
    Sig,
    Key,
    Px,
    Gpos,
    Aaaa,
    Loc,
    Nxt,
    Eid,
    Nimloc,
    Srv,
    Atma,
    Naptr,
    Kx,
    Cert,
    A6,
    Dname,
    Sink,
    Opt,
    Apl,
    Dnskey,
    Ds,
    Rrsig,
    Nsec,
    Dhcid,
    Nsec3,
    Nsec3param,
    Tlsa,
    Hip,
    Ninfo,
    Rkey,
    Talink,
    Cds,
    Cdnskey,
    Openpgpkey,
    Csisc,
    Ta,
    Dlv,
    Other(u16),
}

#[derive(Debug, PartialEq)]
struct DnsHeader {
    id: u16,
    flags: u16,
    qdcount: u16,
    ancount: u16,
    nscount: u16,
    arcount: u16,
}

fn parse_dns_class(input: &[u8]) -> IResult<&[u8], DnsClass> {
    map(be_u16, |value| match value {
        1 => DnsClass::Internet,
        2 => DnsClass::Csnet,
        3 => DnsClass::Chaos,
        4 => DnsClass::Hesiod,
        254 => DnsClass::None,
        255 => DnsClass::Any,
        _ => DnsClass::Other(value),
    })(input)
}

fn parse_dns_type(input: &[u8]) -> IResult<&[u8], DnsType> {
    map(be_u16, |value| match value {
        1 => DnsType::A,
        2 => DnsType::Ns,
        3 => DnsType::Md,
        4 => DnsType::Mf,
        5 => DnsType::Cname,
        6 => DnsType::Soa,
        7 => DnsType::Mb,
        8 => DnsType::Mg,
        9 => DnsType::Mr,
        10 => DnsType::Null,
        11 => DnsType::Wks,
        12 => DnsType::Ptr,
        13 => DnsType::Hinfo,
        14 => DnsType::Minfo,
        15 => DnsType::Mx,
        16 => DnsType::Txt,
        17 => DnsType::Rp,
        18 => DnsType::Afsdb,
        19 => DnsType::X25,
        20 => DnsType::Isdn,
        21 => DnsType::Rt,
        22 => DnsType::Nsap,
        23 => DnsType::NsapPtr,
        24 => DnsType::Sig,
        25 => DnsType::Key,
        26 => DnsType::Px,
        27 => DnsType::Gpos,
        28 => DnsType::Aaaa,
        29 => DnsType::Loc,
        30 => DnsType::Nxt,
        31 => DnsType::Eid,
        32 => DnsType::Nimloc,
        33 => DnsType::Srv,
        34 => DnsType::Atma,
        35 => DnsType::Naptr,
        36 => DnsType::Kx,
        37 => DnsType::Cert,
        38 => DnsType::A6,
        39 => DnsType::Dname,
        40 => DnsType::Sink,
        41 => DnsType::Opt,
        42 => DnsType::Apl,
        43 => DnsType::Dnskey,
        44 => DnsType::Ds,
        45 => DnsType::Rrsig,
        46 => DnsType::Nsec,
        47 => DnsType::Dhcid,
        48 => DnsType::Nsec3,
        49 => DnsType::Nsec3param,
        50 => DnsType::Tlsa,
        51 => DnsType::Hip,
        52 => DnsType::Ninfo,
        53 => DnsType::Rkey,
        54 => DnsType::Talink,
        55 => DnsType::Cds,
        56 => DnsType::Cdnskey,
        57 => DnsType::Openpgpkey,
        58 => DnsType::Csisc,
        59 => DnsType::Ta,
        60 => DnsType::Dlv,
        _ => DnsType::Other(value),
    })(input)
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

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], (&[u8], DnsType, DnsClass)> {
    map(
        tuple((take_while_m_n(1, 255, |c| c != 0), parse_dns_type, parse_dns_class)),
        |(name, dns_type, dns_class)| (name, dns_type, dns_class),
    )(input)
}

fn parse_dns_answer(input: &[u8]) -> IResult<&[u8], (&[u8], DnsType, DnsClass, u32, &[u8])> {
    map(
        tuple((
            take_while_m_n(1, 255, |c| c != 0),
            parse_dns_type,
            parse_dns_class,
            be_u32,
            length_data(be_u16),
        )),
        |(name, dns_type, dns_class, ttl, data)| (name, dns_type, dns_class, ttl, data),
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let file = match File::open(&filename) {
        Ok(file) => file,
        Err(error) => {
            eprintln!("Error opening file {}: {}", filename, error);
            return;
        }
    };

    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).unwrap();

    let header = parse_dns_header(&buffer).unwrap().1;
    println!("{:#?}", header);

    let questions = many1(parse_dns_question)(&buffer[12..])
        .unwrap()
        .1;
    for question in questions {
        println!("{:#?}", question);
    }

    let answers = many1(parse_dns_answer)(&buffer[12 + questions.len() * 5..])
        .unwrap()
        .1;
    for answer in answers {
        println!("{:#?}", answer);
    }
}