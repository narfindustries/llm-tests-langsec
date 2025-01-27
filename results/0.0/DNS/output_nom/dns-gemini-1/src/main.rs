use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, rest},
    number::streaming::{be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::read;
use std::net::IpAddr;
use std::str::from_utf8;

#[derive(Debug)]
enum RdataType {
    A(IpAddr),
    NS,
    CNAME,
    SOA,
    MX,
    TXT,
    AAAA(IpAddr),
    // Add other RDATA types as needed
}

#[derive(Debug)]
struct ResourceRecord {
    name: String,
    rtype: RdataType,
    ttl: u32,
    rclass: u16,
    rdata: Vec<u8>,
}


fn parse_name(input: &[u8]) -> IResult<&[u8], String> {
    let mut name = String::new();
    let mut remaining = input;
    loop {
        let (r, len) = be_u8(remaining)?;
        if len == 0 {
            break;
        }
        let (r2, label) = take(len as usize)(r)?;
        name.push_str(from_utf8(label).unwrap());
        name.push('.');
        remaining = r2;
    }
    Ok((remaining, name.trim_end_matches('.').to_string()))
}

fn be_u8(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1usize), |bytes: &[u8]| bytes[0])(input)
}

fn parse_rdata_a(input: &[u8]) -> IResult<&[u8], RdataType> {
    map_res(take(4usize), |bytes: &[u8]| {
        IpAddr::from(bytes.try_into().unwrap())
    })(input).map(|(rest, ip)| (rest, RdataType::A(ip)))
}

fn parse_rdata_aaaa(input: &[u8]) -> IResult<&[u8], RdataType> {
    map_res(take(16usize), |bytes: &[u8]| {
        IpAddr::from(bytes.try_into().unwrap())
    })(input).map(|(rest, ip)| (rest, RdataType::AAAA(ip)))
}

fn parse_rdata(rtype: u16, input: &[u8]) -> IResult<&[u8], RdataType> {
    match rtype {
        1 => parse_rdata_a(input),
        28 => parse_rdata_aaaa(input),
        _ => Ok((input, RdataType::NS)), // Handle other types as needed
    }
}

fn parse_resource_record(input: &[u8]) -> IResult<&[u8], ResourceRecord> {
    let (input, name) = parse_name(input)?;
    let (input, (rtype, rclass, ttl, rdlength)) = tuple((be_u16, be_u16, be_u32, be_u16))(input)?;
    let (input, rdata) = take(rdlength as usize)(input)?;
    let rtype_data = parse_rdata(rtype, rdata)?;
    Ok((input, ResourceRecord { name, rtype: rtype_data.1, ttl, rclass, rdata: rdata.to_vec() }))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = match read(filename) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    match parse_resource_record(&data) {
        Ok((_, rr)) => println!("{:?}", rr),
        Err(e) => eprintln!("Error parsing DNS record: {:?}", e),
    }
}
