use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, rest},
    error::ErrorKind,
    number::complete::le_u16,
    sequence::tuple,
    IResult,
};
use std::fs::read;
use std::net::IpAddr;
use std::str::from_utf8;


#[derive(Debug)]
enum IcmpType {
    EchoRequest,
    EchoReply,
    DestinationUnreachable(u8),
    TimeExceeded(u8),
    ParameterProblem(u8),
    // Add other ICMP types as needed
    Unknown(u8),
}

#[derive(Debug)]
struct IcmpHeader {
    typ: IcmpType,
    code: u8,
    checksum: u16,
    // Add other header fields as needed based on ICMP type
    data: Vec<u8>,
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, (typ, code, checksum)) = tuple((
        map(take(1usize), |b: &[u8]| match b[0] {
            8 => IcmpType::EchoRequest,
            0 => IcmpType::EchoReply,
            3 => IcmpType::DestinationUnreachable(0), // Placeholder code
            11 => IcmpType::TimeExceeded(0), // Placeholder code
            12 => IcmpType::ParameterProblem(0), // Placeholder code
            x => IcmpType::Unknown(x),
        }),
        take(1usize),
        le_u16,
    ))(input)?;

    let (input, data) = rest(input)?;

    Ok((
        input,
        IcmpHeader {
            typ,
            code: code[0],
            checksum,
            data: data.to_vec(),
        },
    ))
}


fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    match read(filename) {
        Ok(buffer) => {
            match parse_icmp_header(&buffer) {
                Ok((_, header)) => {
                    println!("{:?}", header);
                }
                Err(e) => {
                    eprintln!("Error parsing ICMP header: {:?}", e);
                }
            }
        }
        Err(e) => {
            eprintln!("Error reading file: {}", e);
        }
    }
}
