use nom::bytes::complete::take;
use nom::number::complete::{be_u16, be_u32, be_u8};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest,
    TimeExceeded,
    ParameterProblem,
    Timestamp,
    TimestampReply,
    InformationRequest,
    InformationReply,
    Unknown(u8),
}

#[derive(Debug)]
pub enum DestUnreachableCode {
    NetUnreachable,
    HostUnreachable,
    ProtocolUnreachable,
    PortUnreachable,
    FragmentationNeeded,
    SourceRouteFailed,
    Unknown(u8),
}

#[derive(Debug)]
pub enum RedirectCode {
    RedirectNetwork,
    RedirectHost,
    RedirectTosNetwork,
    RedirectTosHost,
    Unknown(u8),
}

#[derive(Debug)]
pub enum TimeExceededCode {
    TtlExceeded,
    FragmentReassemblyTimeExceeded,
    Unknown(u8),
}

#[derive(Debug)]
pub struct IcmpHeader {
    icmp_type: IcmpType,
    code: u8,
    checksum: u16,
    rest_of_header: RestOfHeader,
}

#[derive(Debug)]
pub enum RestOfHeader {
    EchoData {
        identifier: u16,
        sequence_number: u16,
    },
    ParameterProblem {
        pointer: u8,
        unused: [u8; 3],
    },
    Redirect {
        gateway_address: u32,
    },
    Timestamp {
        identifier: u16,
        sequence_number: u16,
    },
    Generic(u32),
}

fn parse_icmp_type(input: u8) -> IcmpType {
    match input {
        0 => IcmpType::EchoReply,
        3 => IcmpType::DestinationUnreachable,
        4 => IcmpType::SourceQuench,
        5 => IcmpType::Redirect,
        8 => IcmpType::EchoRequest,
        11 => IcmpType::TimeExceeded,
        12 => IcmpType::ParameterProblem,
        13 => IcmpType::Timestamp,
        14 => IcmpType::TimestampReply,
        15 => IcmpType::InformationRequest,
        16 => IcmpType::InformationReply,
        n => IcmpType::Unknown(n),
    }
}

fn parse_dest_unreachable_code(input: u8) -> DestUnreachableCode {
    match input {
        0 => DestUnreachableCode::NetUnreachable,
        1 => DestUnreachableCode::HostUnreachable,
        2 => DestUnreachableCode::ProtocolUnreachable,
        3 => DestUnreachableCode::PortUnreachable,
        4 => DestUnreachableCode::FragmentationNeeded,
        5 => DestUnreachableCode::SourceRouteFailed,
        n => DestUnreachableCode::Unknown(n),
    }
}

fn parse_redirect_code(input: u8) -> RedirectCode {
    match input {
        0 => RedirectCode::RedirectNetwork,
        1 => RedirectCode::RedirectHost,
        2 => RedirectCode::RedirectTosNetwork,
        3 => RedirectCode::RedirectTosHost,
        n => RedirectCode::Unknown(n),
    }
}

fn parse_time_exceeded_code(input: u8) -> TimeExceededCode {
    match input {
        0 => TimeExceededCode::TtlExceeded,
        1 => TimeExceededCode::FragmentReassemblyTimeExceeded,
        n => TimeExceededCode::Unknown(n),
    }
}

fn parse_rest_of_header<'a>(icmp_type: &IcmpType, input: &'a [u8]) -> IResult<&'a [u8], RestOfHeader> {
    match icmp_type {
        IcmpType::EchoReply | IcmpType::EchoRequest => {
            let (input, identifier) = be_u16(input)?;
            let (input, sequence_number) = be_u16(input)?;
            Ok((
                input,
                RestOfHeader::EchoData {
                    identifier,
                    sequence_number,
                },
            ))
        }
        IcmpType::ParameterProblem => {
            let (input, pointer) = be_u8(input)?;
            let (input, unused) = take(3u8)(input)?;
            Ok((
                input,
                RestOfHeader::ParameterProblem {
                    pointer,
                    unused: [unused[0], unused[1], unused[2]],
                },
            ))
        }
        IcmpType::Redirect => {
            let (input, gateway_address) = be_u32(input)?;
            Ok((input, RestOfHeader::Redirect { gateway_address }))
        }
        IcmpType::Timestamp | IcmpType::TimestampReply => {
            let (input, identifier) = be_u16(input)?;
            let (input, sequence_number) = be_u16(input)?;
            Ok((
                input,
                RestOfHeader::Timestamp {
                    identifier,
                    sequence_number,
                },
            ))
        }
        _ => {
            let (input, value) = be_u32(input)?;
            Ok((input, RestOfHeader::Generic(value)))
        }
    }
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, type_value) = be_u8(input)?;
    let icmp_type = parse_icmp_type(type_value);
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = parse_rest_of_header(&icmp_type, input)?;

    Ok((
        input,
        IcmpHeader {
            icmp_type,
            code,
            checksum,
            rest_of_header,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_icmp_header(&buffer) {
        Ok((remaining, header)) => {
            println!("ICMP Header: {:?}", header);
            println!("Remaining data: {} bytes", remaining.len());
        }
        Err(e) => eprintln!("Failed to parse ICMP header: {:?}", e),
    }
}