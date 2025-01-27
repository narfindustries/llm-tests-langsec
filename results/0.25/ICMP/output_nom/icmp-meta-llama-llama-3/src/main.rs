use nom::{
    bytes::complete::{tag, take},
    combinator::{flat_map, map},
    error::{context, ErrorKind},
    multi::{take_till, take_while_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{tuple, preceded},
    IResult,
};
use std::{env, fs, io};

#[derive(Debug)]
enum IcmpType {
    EchoReply = 0,
    DestinationUnreachable = 3,
    SourceQuench = 4,
    Redirect = 5,
    EchoRequest = 8,
    TimeExceeded = 11,
    ParameterProblem = 12,
    Timestamp = 13,
    TimestampReply = 14,
    InfoRequest = 15,
    InfoReply = 16,
    AddressMaskRequest = 17,
    AddressMaskReply = 18,
}

impl IcmpType {
    fn parse(i: &[u8]) -> IResult<&[u8], IcmpType> {
        map(be_u8, |t: u8| match t {
            0 => IcmpType::EchoReply,
            3 => IcmpType::DestinationUnreachable,
            4 => IcmpType::SourceQuench,
            5 => IcmpType::Redirect,
            8 => IcmpType::EchoRequest,
            11 => IcmpType::TimeExceeded,
            12 => IcmpType::ParameterProblem,
            13 => IcmpType::Timestamp,
            14 => IcmpType::TimestampReply,
            15 => IcmpType::InfoRequest,
            16 => IcmpType::InfoReply,
            17 => IcmpType::AddressMaskRequest,
            18 => IcmpType::AddressMaskReply,
            _ => panic!("Invalid ICMP type"),
        })(i)
    }
}

#[derive(Debug)]
enum IcmpCode {
    NetworkUnreachable = 0,
    HostUnreachable = 1,
    ProtocolUnreachable = 2,
    PortUnreachable = 3,
    FragmentationNeeded = 4,
    SourceRouteFailed = 5,
    DestinationNetworkUnknown = 6,
    DestinationHostUnknown = 7,
    SourceHostIsolated = 8,
    NetworkAdministrativelyProhibited = 9,
    HostAdministrativelyProhibited = 10,
    NetworkUnreachableForTos = 11,
    HostUnreachableForTos = 12,
    CommunicationAdministrativelyProhibited = 13,
    HostPrecedenceViolation = 14,
    PrecedenceCutoffInEffect = 15,
}

impl IcmpCode {
    fn parse(i: &[u8]) -> IResult<&[u8], IcmpCode> {
        map(be_u8, |c: u8| match c {
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
            11 => IcmpCode::NetworkUnreachableForTos,
            12 => IcmpCode::HostUnreachableForTos,
            13 => IcmpCode::CommunicationAdministrativelyProhibited,
            14 => IcmpCode::HostPrecedenceViolation,
            15 => IcmpCode::PrecedenceCutoffInEffect,
            _ => panic!("Invalid ICMP code"),
        })(i)
    }
}

#[derive(Debug)]
struct IcmpHeader {
    icmp_type: IcmpType,
    icmp_code: IcmpCode,
    checksum: u16,
    identifier: u16,
    sequence_number: u16,
}

impl IcmpHeader {
    fn parse(i: &[u8]) -> IResult<&[u8], IcmpHeader> {
        context(
            "ICMP header",
            tuple((
                IcmpType::parse,
                IcmpCode::parse,
                be_u16,
                be_u16,
                be_u16,
            )),
        )
        .map(|(i, (icmp_type, icmp_code, checksum, identifier, sequence_number))| {
            (
                i,
                IcmpHeader {
                    icmp_type,
                    icmp_code,
                    checksum,
                    identifier,
                    sequence_number,
                },
            )
        })(i)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let data = match fs::read(input_file) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };
    match IcmpHeader::parse(&data) {
        Ok((remaining, header)) => println!("ICMP Header: {:?}", header),
        Err(e) => eprintln!("Error parsing ICMP header: {:?}", e),
    }
}