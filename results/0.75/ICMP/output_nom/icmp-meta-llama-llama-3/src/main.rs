use nom::{
    bits::complete::{tag, take},
    bytes::complete::{tag as byte_tag, take as byte_take},
    combinator::{map, opt},
    error::{Error, ErrorKind},
    IResult,
};
use std::{env, fs};

#[derive(Debug, PartialEq)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest,
    TimeExceeded,
    ParameterProblem,
    Other(u8),
}

impl From<u8> for IcmpType {
    fn from(value: u8) -> Self {
        match value {
            0 => IcmpType::EchoReply,
            3 => IcmpType::DestinationUnreachable,
            4 => IcmpType::SourceQuench,
            5 => IcmpType::Redirect,
            8 => IcmpType::EchoRequest,
            11 => IcmpType::TimeExceeded,
            12 => IcmpType::ParameterProblem,
            _ => IcmpType::Other(value),
        }
    }
}

#[derive(Debug, PartialEq)]
enum DestinationUnreachableCode {
    NetworkUnreachable,
    HostUnreachable,
    ProtocolUnreachable,
    PortUnreachable,
    FragmentationNeeded,
    SourceRouteFailed,
    Other(u8),
}

impl From<u8> for DestinationUnreachableCode {
    fn from(value: u8) -> Self {
        match value {
            0 => DestinationUnreachableCode::NetworkUnreachable,
            1 => DestinationUnreachableCode::HostUnreachable,
            2 => DestinationUnreachableCode::ProtocolUnreachable,
            3 => DestinationUnreachableCode::PortUnreachable,
            4 => DestinationUnreachableCode::FragmentationNeeded,
            5 => DestinationUnreachableCode::SourceRouteFailed,
            _ => DestinationUnreachableCode::Other(value),
        }
    }
}

#[derive(Debug, PartialEq)]
enum RedirectCode {
    RedirectDatagramsForNetwork,
    RedirectDatagramsForHost,
    RedirectDatagramsForTypeOfServiceAndNetwork,
    RedirectDatagramsForTypeOfServiceAndHost,
    Other(u8),
}

impl From<u8> for RedirectCode {
    fn from(value: u8) -> Self {
        match value {
            0 => RedirectCode::RedirectDatagramsForNetwork,
            1 => RedirectCode::RedirectDatagramsForHost,
            2 => RedirectCode::RedirectDatagramsForTypeOfServiceAndNetwork,
            3 => RedirectCode::RedirectDatagramsForTypeOfServiceAndHost,
            _ => RedirectCode::Other(value),
        }
    }
}

#[derive(Debug, PartialEq)]
enum TimeExceededCode {
    TimeToLiveExceededInTransit,
    FragmentReassemblyTimeExceeded,
    Other(u8),
}

impl From<u8> for TimeExceededCode {
    fn from(value: u8) -> Self {
        match value {
            0 => TimeExceededCode::TimeToLiveExceededInTransit,
            1 => TimeExceededCode::FragmentReassemblyTimeExceeded,
            _ => TimeExceededCode::Other(value),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ParameterProblemCode {
    PointerIndicatesError,
    MissingRequiredOption,
    BadLength,
    Other(u8),
}

impl From<u8> for ParameterProblemCode {
    fn from(value: u8) -> Self {
        match value {
            0 => ParameterProblemCode::PointerIndicatesError,
            1 => ParameterProblemCode::MissingRequiredOption,
            2 => ParameterProblemCode::BadLength,
            _ => ParameterProblemCode::Other(value),
        }
    }
}

#[derive(Debug, PartialEq)]
struct IcmpHeader {
    icmp_type: IcmpType,
    code: u8,
    checksum: u16,
    identifier: u16,
    sequence_number: u16,
}

fn icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, icmp_type) = byte_take(1usize)(input)?;
    let (input, code) = byte_take(1usize)(input)?;
    let (input, checksum) = byte_take(2usize)(input)?;
    let (input, identifier) = byte_take(2usize)(input)?;
    let (input, sequence_number) = byte_take(2usize)(input)?;

    let icmp_type = IcmpType::from(icmp_type[0]);
    let checksum = u16::from_be_bytes([checksum[0], checksum[1]]);
    let identifier = u16::from_be_bytes([identifier[0], identifier[1]]);
    let sequence_number = u16::from_be_bytes([sequence_number[0], sequence_number[1]]);

    Ok((input, IcmpHeader {
        icmp_type,
        code: code[0],
        checksum,
        identifier,
        sequence_number,
    }))
}

#[derive(Debug, PartialEq)]
struct DestinationUnreachable {
    icmp_header: IcmpHeader,
    unused: u32,
    next_hop_mtu: u16,
}

fn destination_unreachable(input: &[u8]) -> IResult<&[u8], DestinationUnreachable> {
    let (input, icmp_header) = icmp_header(input)?;
    let (input, unused) = byte_take(4usize)(input)?;
    let (input, next_hop_mtu) = byte_take(2usize)(input)?;

    let unused = u32::from_be_bytes([unused[0], unused[1], unused[2], unused[3]]);
    let next_hop_mtu = u16::from_be_bytes([next_hop_mtu[0], next_hop_mtu[1]]);

    Ok((input, DestinationUnreachable {
        icmp_header,
        unused,
        next_hop_mtu,
    }))
}

#[derive(Debug, PartialEq)]
struct Redirect {
    icmp_header: IcmpHeader,
    gateway_address: u32,
}

fn redirect(input: &[u8]) -> IResult<&[u8], Redirect> {
    let (input, icmp_header) = icmp_header(input)?;
    let (input, gateway_address) = byte_take(4usize)(input)?;

    let gateway_address = u32::from_be_bytes([gateway_address[0], gateway_address[1], gateway_address[2], gateway_address[3]]);

    Ok((input, Redirect {
        icmp_header,
        gateway_address,
    }))
}

#[derive(Debug, PartialEq)]
struct TimeExceeded {
    icmp_header: IcmpHeader,
    unused: u32,
}

fn time_exceeded(input: &[u8]) -> IResult<&[u8], TimeExceeded> {
    let (input, icmp_header) = icmp_header(input)?;
    let (input, unused) = byte_take(4usize)(input)?;

    let unused = u32::from_be_bytes([unused[0], unused[1], unused[2], unused[3]]);

    Ok((input, TimeExceeded {
        icmp_header,
        unused,
    }))
}

#[derive(Debug, PartialEq)]
struct ParameterProblem {
    icmp_header: IcmpHeader,
    pointer: u8,
}

fn parameter_problem(input: &[u8]) -> IResult<&[u8], ParameterProblem> {
    let (input, icmp_header) = icmp_header(input)?;
    let (input, pointer) = byte_take(1usize)(input)?;

    Ok((input, ParameterProblem {
        icmp_header,
        pointer: pointer[0],
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let input_file = &args[1];
    let input_data = fs::read(input_file).unwrap();

    let (input, icmp_header) = icmp_header(&input_data).unwrap();

    match icmp_header.icmp_type {
        IcmpType::DestinationUnreachable => {
            let (_, destination_unreachable) = destination_unreachable(&input_data).unwrap();
            println!("{:?}", destination_unreachable);
        }
        IcmpType::Redirect => {
            let (_, redirect) = redirect(&input_data).unwrap();
            println!("{:?}", redirect);
        }
        IcmpType::TimeExceeded => {
            let (_, time_exceeded) = time_exceeded(&input_data).unwrap();
            println!("{:?}", time_exceeded);
        }
        IcmpType::ParameterProblem => {
            let (_, parameter_problem) = parameter_problem(&input_data).unwrap();
            println!("{:?}", parameter_problem);
        }
        _ => {
            println!("{:?}", icmp_header);
        }
    }
}