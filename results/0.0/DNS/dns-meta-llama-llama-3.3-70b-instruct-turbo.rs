use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{length_data, many_till},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, BufReader},
    path::Path,
};

#[derive(Debug)]
enum DnsClass {
    Internet,
    Csnet,
    Chaos,
    Hesiod,
    None,
    Any,
    Unassigned(u16),
}

#[derive(Debug)]
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
    Ds,
    Sshfp,
    Ipseckey,
    Rrsig,
    Nsec,
    Dnskey,
    Dhcidd,
    Nsec3,
    Nsec3param,
    Tlsa,
    Smimea,
    Unassigned(u16),
}

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
    name: Vec<String>,
    qtype: DnsType,
    qclass: DnsClass,
}

#[derive(Debug)]
struct DnsRr {
    name: Vec<String>,
    rtype: DnsType,
    rclass: DnsClass,
    ttl: u32,
    rdlength: u16,
    rdata: Vec<u8>,
}

fn parse_dns_class(input: &[u8]) -> IResult<&[u8], DnsClass> {
    map_res(be_u16, |x| match x {
        1 => Ok(DnsClass::Internet),
        2 => Ok(DnsClass::Csnet),
        3 => Ok(DnsClass::Chaos),
        4 => Ok(DnsClass::Hesiod),
        254 => Ok(DnsClass::None),
        255 => Ok(DnsClass::Any),
        _ => Ok(DnsClass::Unassigned(x)),
    })(input)
}

fn parse_dns_type(input: &[u8]) -> IResult<&[u8], DnsType> {
    map_res(be_u16, |x| match x {
        1 => Ok(DnsType::A),
        2 => Ok(DnsType::Ns),
        3 => Ok(DnsType::Md),
        4 => Ok(DnsType::Mf),
        5 => Ok(DnsType::Cname),
        6 => Ok(DnsType::Soa),
        7 => Ok(DnsType::Mb),
        8 => Ok(DnsType::Mg),
        9 => Ok(DnsType::Mr),
        10 => Ok(DnsType::Null),
        11 => Ok(DnsType::Wks),
        12 => Ok(DnsType::Ptr),
        13 => Ok(DnsType::Hinfo),
        14 => Ok(DnsType::Minfo),
        15 => Ok(DnsType::Mx),
        16 => Ok(DnsType::Txt),
        17 => Ok(DnsType::Rp),
        18 => Ok(DnsType::Afsdb),
        19 => Ok(DnsType::X25),
        20 => Ok(DnsType::Isdn),
        21 => Ok(DnsType::Rt),
        22 => Ok(DnsType::Nsap),
        23 => Ok(DnsType::NsapPtr),
        24 => Ok(DnsType::Sig),
        25 => Ok(DnsType::Key),
        26 => Ok(DnsType::Px),
        27 => Ok(DnsType::Gpos),
        28 => Ok(DnsType::Aaaa),
        29 => Ok(DnsType::Loc),
        30 => Ok(DnsType::Nxt),
        31 => Ok(DnsType::Eid),
        32 => Ok(DnsType::Nimloc),
        33 => Ok(DnsType::Srv),
        34 => Ok(DnsType::Atma),
        35 => Ok(DnsType::Naptr),
        36 => Ok(DnsType::Kx),
        37 => Ok(DnsType::Cert),
        38 => Ok(DnsType::A6),
        39 => Ok(DnsType::Dname),
        40 => Ok(DnsType::Sink),
        41 => Ok(DnsType::Opt),
        42 => Ok(DnsType::Apl),
        43 => Ok(DnsType::Ds),
        44 => Ok(DnsType::Sshfp),
        45 => Ok(DnsType::Ipseckey),
        46 => Ok(DnsType::Rrsig),
        47 => Ok(DnsType::Nsec),
        48 => Ok(DnsType::Dnskey),
        49 => Ok(DnsType::Dhcidd),
        50 => Ok(DnsType::Nsec3),
        51 => Ok(DnsType::Nsec3param),
        52 => Ok(DnsType::Tlsa),
        53 => Ok(DnsType::Smimea),
        _ => Ok(DnsType::Unassigned(x)),
    })(input)
}

fn parse_dns_header(input: &[u8]) -> IResult<&[u8], DnsHeader> {
    let (input, id) = be_u16(input)?;
    let (input, flags) = be_u16(input)?;
    let (input, qdcount) = be_u16(input)?;
    let (input, ancount) = be_u16(input)?;
    let (input, nscount) = be_u16(input)?;
    let (input, arcount) = be_u16(input)?;
    Ok((input, DnsHeader { id, flags, qdcount, ancount, nscount, arcount }))
}

fn parse_dns_question(input: &[u8]) -> IResult<&[u8], DnsQuestion> {
    let (input, name) = many_till(
        map_res(take_while_m_n(1, 63, |x| x != 0), |x| String::from_utf8_lossy(x).into_owned()),
        tag(&[0]),
    )(input)?;
    let (input, qtype) = parse_dns_type(input)?;
    let (input, qclass) = parse_dns_class(input)?;
    Ok((input, DnsQuestion { name, qtype, qclass }))
}

fn parse_dns_rr(input: &[u8]) -> IResult<&[u8], DnsRr> {
    let (input, name) = many_till(
        map_res(take_while_m_n(1, 63, |x| x != 0), |x| String::from_utf8_lossy(x).into_owned()),
        tag(&[0]),
    )(input)?;
    let (input, rtype) = parse_dns_type(input)?;
    let (input, rclass) = parse_dns_class(input)?;
    let (input, ttl) = be_u32(input)?;
    let (input, rdlength) = be_u16(input)?;
    let (input, rdata) = take(rdlength)(input)?;
    Ok((input, DnsRr { name, rtype, rclass, ttl, rdlength, rdata: rdata.to_vec() }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    let mut data = Vec::new();
    reader.read_to_end(&mut data).unwrap();
    let (input, header) = parse_dns_header(&data).unwrap();
    let (input, questions) = many_till(parse_dns_question, tag(&[0]))(input).unwrap();
    let (input, answers) = many_till(parse_dns_rr, tag(&[0]))(input).unwrap();
    println!("{:?}", header);
    println!("{:?}", questions);
    println!("{:?}", answers);
}