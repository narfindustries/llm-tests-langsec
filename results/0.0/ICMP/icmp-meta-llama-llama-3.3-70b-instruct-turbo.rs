use nom::{
    combinator::{map},
    error::{Error, ErrorKind},
    number::complete::{be_u8, be_u16},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    clone::Clone,
};

#[derive(Debug, PartialEq, Clone)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest,
    TimeExceeded,
    InformationRequest,
    InformationReply,
    Unassigned(u8),
}

impl IcmpType {
    fn from_u8(n: u8) -> IcmpType {
        match n {
            0 => IcmpType::EchoReply,
            3 => IcmpType::DestinationUnreachable,
            4 => IcmpType::SourceQuench,
            5 => IcmpType::Redirect,
            8 => IcmpType::EchoRequest,
            11 => IcmpType::TimeExceeded,
            15 => IcmpType::InformationRequest,
            16 => IcmpType::InformationReply,
            _ => IcmpType::Unassigned(n),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
enum IcmpCode {
    NetworkUnreachable,
    HostUnreachable,
    ProtocolUnreachable,
    PortUnreachable,
    FragmentationNeeded,
    SourceRouteFailed,
    Unassigned(u8),
}

impl IcmpCode {
    fn from_u8(n: u8, icmp_type: &IcmpType) -> IcmpCode {
        match icmp_type {
            IcmpType::DestinationUnreachable => match n {
                0 => IcmpCode::NetworkUnreachable,
                1 => IcmpCode::HostUnreachable,
                2 => IcmpCode::ProtocolUnreachable,
                3 => IcmpCode::PortUnreachable,
                4 => IcmpCode::FragmentationNeeded,
                5 => IcmpCode::SourceRouteFailed,
                _ => IcmpCode::Unassigned(n),
            },
            IcmpType::Redirect => match n {
                0 => IcmpCode::NetworkUnreachable,
                1 => IcmpCode::HostUnreachable,
                2 => IcmpCode::ProtocolUnreachable,
                3 => IcmpCode::PortUnreachable,
                _ => IcmpCode::Unassigned(n),
            },
            IcmpType::TimeExceeded => match n {
                0 => IcmpCode::NetworkUnreachable,
                1 => IcmpCode::HostUnreachable,
                _ => IcmpCode::Unassigned(n),
            },
            _ => IcmpCode::Unassigned(n),
        }
    }
}

#[derive(Debug, PartialEq)]
struct IcmpHeader {
    icmp_type: IcmpType,
    code: IcmpCode,
    checksum: u16,
    identifier: u16,
    sequence_number: u16,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], IcmpType> {
    map(be_u8, IcmpType::from_u8)(input)
}

fn parse_icmp_code<'a>(input: &'a [u8], icmp_type: &'a IcmpType) -> IResult<&'a [u8], IcmpCode> {
    map(be_u8, move |n| IcmpCode::from_u8(n, icmp_type))(input)
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = parse_icmp_code(input, &icmp_type)?;
    let (input, checksum) = be_u16(input)?;
    let (input, identifier) = be_u16(input)?;
    let (input, sequence_number) = be_u16(input)?;
    Ok((input, IcmpHeader { icmp_type, code, checksum, identifier, sequence_number }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).expect("Failed to read file");
    match parse_icmp_header(&input) {
        Ok((_remaining, header)) => println!("{:?}", header),
        Err(err) => panic!("Failed to parse ICMP header: {:?}", err),
    }
}