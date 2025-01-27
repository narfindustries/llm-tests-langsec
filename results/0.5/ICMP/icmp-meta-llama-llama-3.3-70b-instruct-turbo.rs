use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    error::{ErrorKind, ParseError},
    multi::take,
    number::complete::{be_u8, be_u16, be_u32},
    IResult,
};
use std::{env, fs, io};
use std::path::Path;

#[derive(Debug, PartialEq)]
enum IcmpType {
    EchoReply = 0,
    DestinationUnreachable = 3,
    SourceQuench = 4,
    Redirect = 5,
    Echo = 8,
    TimeExceeded = 11,
    ParameterProblem = 12,
    Timestamp = 13,
    TimestampReply = 14,
    InformationRequest = 15,
    InformationReply = 16,
    AddressMaskRequest = 17,
    AddressMaskReply = 18,
    Unknown(u8),
}

impl IcmpType {
    fn from_u8(n: u8) -> Self {
        match n {
            0 => IcmpType::EchoReply,
            3 => IcmpType::DestinationUnreachable,
            4 => IcmpType::SourceQuench,
            5 => IcmpType::Redirect,
            8 => IcmpType::Echo,
            11 => IcmpType::TimeExceeded,
            12 => IcmpType::ParameterProblem,
            13 => IcmpType::Timestamp,
            14 => IcmpType::TimestampReply,
            15 => IcmpType::InformationRequest,
            16 => IcmpType::InformationReply,
            17 => IcmpType::AddressMaskRequest,
            18 => IcmpType::AddressMaskReply,
            _ => IcmpType::Unknown(n),
        }
    }
}

#[derive(Debug)]
enum IcmpDestinationUnreachableCode {
    NetUnreachable = 0,
    HostUnreachable = 1,
    ProtocolUnreachable = 2,
    PortUnreachable = 3,
    FragmentationNeeded = 4,
    SourceRouteFailed = 5,
    DestinationNetworkUnknown = 6,
    DestinationHostUnknown = 7,
    SourceHostIsolated = 8,
    CommunicationWithDestinationNetworkAdministrativelyProhibited = 9,
    CommunicationWithDestinationHostAdministrativelyProhibited = 10,
    NetworkUnreachableForTypeOfService = 11,
    HostUnreachableForTypeOfService = 12,
    CommunicationAdministrativelyProhibited = 13,
    HostPrecedenceViolation = 14,
    PrecedenceCutoffInEffect = 15,
    Unknown(u8),
}

impl IcmpDestinationUnreachableCode {
    fn from_u8(n: u8) -> Self {
        match n {
            0 => IcmpDestinationUnreachableCode::NetUnreachable,
            1 => IcmpDestinationUnreachableCode::HostUnreachable,
            2 => IcmpDestinationUnreachableCode::ProtocolUnreachable,
            3 => IcmpDestinationUnreachableCode::PortUnreachable,
            4 => IcmpDestinationUnreachableCode::FragmentationNeeded,
            5 => IcmpDestinationUnreachableCode::SourceRouteFailed,
            6 => IcmpDestinationUnreachableCode::DestinationNetworkUnknown,
            7 => IcmpDestinationUnreachableCode::DestinationHostUnknown,
            8 => IcmpDestinationUnreachableCode::SourceHostIsolated,
            9 => IcmpDestinationUnreachableCode::CommunicationWithDestinationNetworkAdministrativelyProhibited,
            10 => IcmpDestinationUnreachableCode::CommunicationWithDestinationHostAdministrativelyProhibited,
            11 => IcmpDestinationUnreachableCode::NetworkUnreachableForTypeOfService,
            12 => IcmpDestinationUnreachableCode::HostUnreachableForTypeOfService,
            13 => IcmpDestinationUnreachableCode::CommunicationAdministrativelyProhibited,
            14 => IcmpDestinationUnreachableCode::HostPrecedenceViolation,
            15 => IcmpDestinationUnreachableCode::PrecedenceCutoffInEffect,
            _ => IcmpDestinationUnreachableCode::Unknown(n),
        }
    }
}

#[derive(Debug)]
enum IcmpHeader {
    EchoReply { sequence_number: u16, identifier: u16 },
    DestinationUnreachable { unused: u32, next_hop_mtu: u16, code: IcmpDestinationUnreachableCode },
    SourceQuench { unused: u32, identifier: u16, sequence_number: u16 },
    Redirect { address: u32, code: u8 },
    Echo { sequence_number: u16, identifier: u16 },
    TimeExceeded { unused: u32 },
    ParameterProblem { pointer: u8 },
    Timestamp { identifier: u16, sequence_number: u16 },
    TimestampReply { identifier: u16, sequence_number: u16 },
    InformationRequest { identifier: u16, sequence_number: u16 },
    InformationReply { identifier: u16, sequence_number: u16 },
    AddressMaskRequest { identifier: u16, sequence_number: u16 },
    AddressMaskReply { identifier: u16, sequence_number: u16 },
    Unknown,
}

fn parse_icmp_type(i: &[u8]) -> IResult<&[u8], u8> {
    be_u8(i)
}

fn parse_icmp_code(i: &[u8]) -> IResult<&[u8], u8> {
    be_u8(i)
}

fn parse_icmp_checksum(i: &[u8]) -> IResult<&[u8], u16> {
    be_u16(i)
}

fn parse_icmp_identifier(i: &[u8]) -> IResult<&[u8], u16> {
    be_u16(i)
}

fn parse_icmp_sequence_number(i: &[u8]) -> IResult<&[u8], u16> {
    be_u16(i)
}

fn parse_icmp_timestamp_identifier(i: &[u8]) -> IResult<&[u8], u16> {
    be_u16(i)
}

fn parse_icmp_timestamp_sequence_number(i: &[u8]) -> IResult<&[u8], u16> {
    be_u16(i)
}

fn parse_icmp_destination_unreachable_code(i: &[u8]) -> IResult<&[u8], u8> {
    be_u8(i)
}

fn parse_icmp_address_mask(i: &[u8]) -> IResult<&[u8], u32> {
    be_u32(i)
}

fn parse_icmp_time_exceeded(i: &[u8]) -> IResult<&[u8], u32> {
    take(4u8)(i)
}

fn parse_icmp_timestamp(i: &[u8]) -> IResult<&[u8], (u16, u16)> {
    let (i, identifier) = parse_icmp_timestamp_identifier(i)?;
    let (i, sequence_number) = parse_icmp_timestamp_sequence_number(i)?;
    Ok((i, (identifier, sequence_number)))
}

fn parse_icmp_header(i: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (i, icmp_type) = parse_icmp_type(i)?;
    let (i, icmp_code) = parse_icmp_code(i)?;
    let (i, icmp_checksum) = parse_icmp_checksum(i)?;

    match IcmpType::from_u8(icmp_type) {
        IcmpType::EchoReply => {
            let (i, sequence_number) = parse_icmp_sequence_number(i)?;
            let (i, identifier) = parse_icmp_identifier(i)?;
            Ok((i, IcmpHeader::EchoReply { sequence_number, identifier }))
        }
        IcmpType::DestinationUnreachable => {
            let code = IcmpDestinationUnreachableCode::from_u8(icmp_code);
            let (i, unused) = parse_icmp_time_exceeded(i)?;
            let (i, next_hop_mtu) = map(be_u16, |x| x.to_be())(i)?;
            Ok((i, IcmpHeader::DestinationUnreachable { unused, code, next_hop_mtu }))
        }
        IcmpType::SourceQuench => {
            let (i, unused) = parse_icmp_time_exceeded(i)?;
            let (i, identifier) = parse_icmp_identifier(i)?;
            let (i, sequence_number) = parse_icmp_sequence_number(i)?;
            Ok((i, IcmpHeader::SourceQuench { unused, identifier, sequence_number }))
        }
        IcmpType::Redirect => {
            let (i, address) = parse_icmp_address_mask(i)?;
            Ok((i, IcmpHeader::Redirect { address, code: icmp_code }))
        }
        IcmpType::Echo => {
            let (i, sequence_number) = parse_icmp_sequence_number(i)?;
            let (i, identifier) = parse_icmp_identifier(i)?;
            Ok((i, IcmpHeader::Echo { sequence_number, identifier }))
        }
        IcmpType::TimeExceeded => {
            let (i, unused) = parse_icmp_time_exceeded(i)?;
            Ok((i, IcmpHeader::TimeExceeded { unused }))
        }
        IcmpType::ParameterProblem => {
            let (i, pointer) = parse_icmp_code(i)?;
            Ok((i, IcmpHeader::ParameterProblem { pointer }))
        }
        IcmpType::Timestamp => {
            let (i, timestamp) = parse_icmp_timestamp(i)?;
            Ok((i, IcmpHeader::Timestamp { identifier: timestamp.0, sequence_number: timestamp.1 }))
        }
        IcmpType::TimestampReply => {
            let (i, timestamp) = parse_icmp_timestamp(i)?;
            Ok((i, IcmpHeader::TimestampReply { identifier: timestamp.0, sequence_number: timestamp.1 }))
        }
        IcmpType::InformationRequest => {
            let (i, sequence_number) = parse_icmp_sequence_number(i)?;
            let (i, identifier) = parse_icmp_identifier(i)?;
            Ok((i, IcmpHeader::InformationRequest { identifier, sequence_number }))
        }
        IcmpType::InformationReply => {
            let (i, sequence_number) = parse_icmp_sequence_number(i)?;
            let (i, identifier) = parse_icmp_identifier(i)?;
            Ok((i, IcmpHeader::InformationReply { identifier, sequence_number }))
        }
        IcmpType::AddressMaskRequest => {
            let (i, sequence_number) = parse_icmp_sequence_number(i)?;
            let (i, identifier) = parse_icmp_identifier(i)?;
            Ok((i, IcmpHeader::AddressMaskRequest { identifier, sequence_number }))
        }
        IcmpType::AddressMaskReply => {
            let (i, sequence_number) = parse_icmp_sequence_number(i)?;
            let (i, identifier) = parse_icmp_identifier(i)?;
            Ok((i, IcmpHeader::AddressMaskReply { identifier, sequence_number }))
        }
        IcmpType::Unknown(_) => Ok((i, IcmpHeader::Unknown)),
    }
}

fn parse_icmp(i: &[u8]) -> IResult<&[u8], IcmpHeader> {
    parse_icmp_header(i)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }
    let filename = &args[1];
    let path = Path::new(filename);
    let data = match fs::read(path) {
        Err(why) => panic!("couldn't read {}: {}", path.display(), why),
        Ok(data) => data,
    };
    match parse_icmp(&data) {
        Ok((i, header)) => println!("{:?}", header),
        Err(err) => println!("Error parsing icmp: {:?}", err),
    }
}