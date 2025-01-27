use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::map;
use nom::error::ErrorKind;
use nom::multi::take_while_m_n;
use nom::number::complete::{be_u16, be_u32, be_u8};
use nom::sequence::tuple;
use nom::{Finish, IResult};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest,
    TimeExceeded,
    ParameterProblem,
    TimestampRequest,
    TimestampReply,
    InfoRequest,
    InfoReply,
    AddressRequest,
    AddressReply,
    Unknown(u8),
}

impl IcmpType {
    fn from_u8(n: u8) -> Self {
        match n {
            0 => IcmpType::EchoReply,
            1 => IcmpType::DestinationUnreachable,
            2 => IcmpType::SourceQuench,
            3 => IcmpType::Redirect,
            4 => IcmpType::EchoRequest,
            5 => IcmpType::TimeExceeded,
            6 => IcmpType::ParameterProblem,
            7 => IcmpType::TimestampRequest,
            8 => IcmpType::TimestampReply,
            9 => IcmpType::InfoRequest,
            10 => IcmpType::InfoReply,
            11 => IcmpType::AddressRequest,
            12 => IcmpType::AddressReply,
            _ => IcmpType::Unknown(n),
        }
    }
}

#[derive(Debug, PartialEq)]
enum IcmpCode {
    NetworkUnreachable,
    HostUnreachable,
    ProtocolUnreachable,
    PortUnreachable,
    FragmentationNeeded,
    SourceRouteFailed,
    DestinationNetworkUnknown,
    DestinationHostUnknown,
    SourceHostIsolated,
    NetworkAdministrativelyProhibited,
    HostAdministrativelyProhibited,
    NetworkUnreachableForToS,
    HostUnreachableForToS,
    CommunicationAdministrativelyProhibited,
    HostPrecedenceViolation,
    PrecedenceCutoffInEffect,
    Unknown(u8),
}

impl IcmpCode {
    fn from_u8(n: u8, icmp_type: IcmpType) -> Self {
        match icmp_type {
            IcmpType::DestinationUnreachable => match n {
                0 => IcmpCode::NetworkUnreachable,
                1 => IcmpCode::HostUnreachable,
                2 => IcmpCode::ProtocolUnreachable,
                3 => IcmpCode::PortUnreachable,
                4 => IcmpCode::FragmentationNeeded,
                5 => IcmpCode::SourceRouteFailed,
                6 => IcmpCode::DestinationNetworkUnknown,
                7 => IcmpCode::DestinationHostUnknown,
                8 => IcmpCode::SourceHostIsolated,
                9 => IcmpCode::NetworkAdministrativelyProhibited,
                10 => IcmpCode::HostAdministrativelyProhibited,
                11 => IcmpCode::NetworkUnreachableForToS,
                12 => IcmpCode::HostUnreachableForToS,
                13 => IcmpCode::CommunicationAdministrativelyProhibited,
                14 => IcmpCode::HostPrecedenceViolation,
                15 => IcmpCode::PrecedenceCutoffInEffect,
                _ => IcmpCode::Unknown(n),
            },
            _ => IcmpCode::Unknown(n),
        }
    }
}

#[derive(Debug, PartialEq)]
struct IcmpHeader {
    icmp_type: IcmpType,
    icmp_code: IcmpCode,
    checksum: u16,
    identifier: u16,
    sequence_number: u16,
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, (icmp_type, icmp_code, checksum)) = tuple((be_u8, be_u8, be_u16))(input)?;
    let (input, (identifier, sequence_number)) = tuple((be_u16, be_u16))(input)?;
    Ok((
        input,
        IcmpHeader {
            icmp_type: IcmpType::from_u8(icmp_type),
            icmp_code: IcmpCode::from_u8(icmp_code, IcmpType::from_u8(icmp_type)),
            checksum,
            identifier,
            sequence_number,
        },
    ))
}

fn parse_icmp_distance(input: &[u8]) -> IResult<&[u8], u32> {
    let (input, data) = take(4u8)(input)?;
    Ok((input, u32::from_be_bytes(data.try_into().unwrap())))
}

fn parse_icmp_gateway(input: &[u8]) -> IResult<&[u8], [u8; 4]> {
    let (input, data) = take(4u8)(input)?;
    Ok((input, data.try_into().unwrap()))
}

fn parse_icmp_pointer(input: &[u8]) -> IResult<&[u8], u8> {
    let (input, data) = be_u8(input)?;
    Ok((input, data))
}

fn parse_icmp_header_and_data(input: &[u8]) -> IResult<&[u8], (IcmpHeader, Vec<u8>)> {
    let (input, icmp_header) = parse_icmp_header(input)?;
    let (input, data) = take_while_m_n(0, 1024, |x| x != 0)(input)?;
    Ok((input, (icmp_header, data.iter().cloned().collect())))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut input = Vec::new();
    file.read_to_end(&mut input).expect("Failed to read file");

    match parse_icmp_header_and_data(&input) {
        Ok((remaining, (icmp_header, data))) => {
            println!("ICMP Header: {:?}", icmp_header);
            println!("Data: {:?}", data);
            if !remaining.is_empty() {
                println!("Unparsed Data: {:?}", remaining);
            }
        }
        Err(err) => {
            eprintln!("Error parsing ICMP packet: {:?}", err);
        }
    }
}