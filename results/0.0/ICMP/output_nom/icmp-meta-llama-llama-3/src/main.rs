use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::take,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Result},
};

#[derive(Debug)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest,
    TimeExceeded,
    ParameterProblem,
    Timestamp,
    TimestampReply,
    InfoRequest,
    InfoReply,
    AddressMaskRequest,
    AddressMaskReply,
    Traceroute,
}

#[derive(Debug)]
struct IcmpHeader {
    icmp_type: IcmpType,
    code: u8,
    checksum: u16,
    identifier: u16,
    sequence_number: u16,
}

#[derive(Debug)]
struct IcmpEcho {
    header: IcmpHeader,
    timestamp: u32,
    data: Vec<u8>,
}

#[derive(Debug)]
struct IcmpDestinationUnreachable {
    header: IcmpHeader,
    unused: u32,
    ip_header: Vec<u8>,
}

#[derive(Debug)]
struct IcmpSourceQuench {
    header: IcmpHeader,
    unused: u32,
    ip_header: Vec<u8>,
}

#[derive(Debug)]
struct IcmpRedirect {
    header: IcmpHeader,
    gateway_address: u32,
    ip_header: Vec<u8>,
}

#[derive(Debug)]
struct IcmpTimeExceeded {
    header: IcmpHeader,
    unused: u32,
    ip_header: Vec<u8>,
}

#[derive(Debug)]
struct IcmpParameterProblem {
    header: IcmpHeader,
    pointer: u8,
    unused: u32,
    ip_header: Vec<u8>,
}

#[derive(Debug)]
struct IcmpTimestamp {
    header: IcmpHeader,
    originate_timestamp: u32,
    receive_timestamp: u32,
    transmit_timestamp: u32,
}

#[derive(Debug)]
struct IcmpTimestampReply {
    header: IcmpHeader,
    originate_timestamp: u32,
    receive_timestamp: u32,
    transmit_timestamp: u32,
}

#[derive(Debug)]
struct IcmpInfoRequest {
    header: IcmpHeader,
    unused: u32,
}

#[derive(Debug)]
struct IcmpInfoReply {
    header: IcmpHeader,
    unused: u32,
}

#[derive(Debug)]
struct IcmpAddressMaskRequest {
    header: IcmpHeader,
    address: u32,
}

#[derive(Debug)]
struct IcmpAddressMaskReply {
    header: IcmpHeader,
    address: u32,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], IcmpType> {
    map(
        take(1u8),
        |input: &[u8]| match input[0] {
            0 => IcmpType::EchoReply,
            1 => IcmpType::DestinationUnreachable,
            2 => IcmpType::SourceQuench,
            3 => IcmpType::Redirect,
            4 => IcmpType::EchoRequest,
            5 => IcmpType::TimeExceeded,
            6 => IcmpType::ParameterProblem,
            7 => IcmpType::Timestamp,
            8 => IcmpType::TimestampReply,
            9 => IcmpType::InfoRequest,
            10 => IcmpType::InfoReply,
            11 => IcmpType::AddressMaskRequest,
            12 => IcmpType::AddressMaskReply,
            13 => IcmpType::Traceroute,
            _ => panic!("Invalid ICMP type"),
        },
    )(input)
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = take(1u8)(input)?;
    let (input, checksum) = take(2u8)(input)?;
    let (input, identifier) = take(2u8)(input)?;
    let (input, sequence_number) = take(2u8)(input)?;
    Ok((
        input,
        IcmpHeader {
            icmp_type,
            code: code[0],
            checksum: u16::from_be_bytes([checksum[0], checksum[1]]),
            identifier: u16::from_be_bytes([identifier[0], identifier[1]]),
            sequence_number: u16::from_be_bytes([sequence_number[0], sequence_number[1]]),
        },
    ))
}

fn parse_icmp_echo(input: &[u8]) -> IResult<&[u8], IcmpEcho> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, timestamp) = take(4u8)(input)?;
    let (input, data) = take(input.len() - 8)(input)?;
    Ok((
        input,
        IcmpEcho {
            header,
            timestamp: u32::from_be_bytes([timestamp[0], timestamp[1], timestamp[2], timestamp[3]]),
            data: data.to_vec(),
        },
    ))
}

fn parse_icmp_destination_unreachable(input: &[u8]) -> IResult<&[u8], IcmpDestinationUnreachable> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, unused) = take(4u8)(input)?;
    let (input, ip_header) = take(28u8)(input)?;
    Ok((
        input,
        IcmpDestinationUnreachable {
            header,
            unused: u32::from_be_bytes([unused[0], unused[1], unused[2], unused[3]]),
            ip_header: ip_header.to_vec(),
        },
    ))
}

fn parse_icmp_source_quench(input: &[u8]) -> IResult<&[u8], IcmpSourceQuench> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, unused) = take(4u8)(input)?;
    let (input, ip_header) = take(28u8)(input)?;
    Ok((
        input,
        IcmpSourceQuench {
            header,
            unused: u32::from_be_bytes([unused[0], unused[1], unused[2], unused[3]]),
            ip_header: ip_header.to_vec(),
        },
    ))
}

fn parse_icmp_redirect(input: &[u8]) -> IResult<&[u8], IcmpRedirect> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, gateway_address) = take(4u8)(input)?;
    let (input, ip_header) = take(28u8)(input)?;
    Ok((
        input,
        IcmpRedirect {
            header,
            gateway_address: u32::from_be_bytes([gateway_address[0], gateway_address[1], gateway_address[2], gateway_address[3]]),
            ip_header: ip_header.to_vec(),
        },
    ))
}

fn parse_icmp_time_exceeded(input: &[u8]) -> IResult<&[u8], IcmpTimeExceeded> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, unused) = take(4u8)(input)?;
    let (input, ip_header) = take(28u8)(input)?;
    Ok((
        input,
        IcmpTimeExceeded {
            header,
            unused: u32::from_be_bytes([unused[0], unused[1], unused[2], unused[3]]),
            ip_header: ip_header.to_vec(),
        },
    ))
}

fn parse_icmp_parameter_problem(input: &[u8]) -> IResult<&[u8], IcmpParameterProblem> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, pointer) = take(1u8)(input)?;
    let (input, unused) = take(3u8)(input)?;
    let (input, ip_header) = take(28u8)(input)?;
    Ok((
        input,
        IcmpParameterProblem {
            header,
            pointer: pointer[0],
            unused: u32::from_be_bytes([unused[0], unused[1], unused[2], 0]),
            ip_header: ip_header.to_vec(),
        },
    ))
}

fn parse_icmp_timestamp(input: &[u8]) -> IResult<&[u8], IcmpTimestamp> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, originate_timestamp) = take(4u8)(input)?;
    let (input, receive_timestamp) = take(4u8)(input)?;
    let (input, transmit_timestamp) = take(4u8)(input)?;
    Ok((
        input,
        IcmpTimestamp {
            header,
            originate_timestamp: u32::from_be_bytes([originate_timestamp[0], originate_timestamp[1], originate_timestamp[2], originate_timestamp[3]]),
            receive_timestamp: u32::from_be_bytes([receive_timestamp[0], receive_timestamp[1], receive_timestamp[2], receive_timestamp[3]]),
            transmit_timestamp: u32::from_be_bytes([transmit_timestamp[0], transmit_timestamp[1], transmit_timestamp[2], transmit_timestamp[3]]),
        },
    ))
}

fn parse_icmp_timestamp_reply(input: &[u8]) -> IResult<&[u8], IcmpTimestampReply> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, originate_timestamp) = take(4u8)(input)?;
    let (input, receive_timestamp) = take(4u8)(input)?;
    let (input, transmit_timestamp) = take(4u8)(input)?;
    Ok((
        input,
        IcmpTimestampReply {
            header,
            originate_timestamp: u32::from_be_bytes([originate_timestamp[0], originate_timestamp[1], originate_timestamp[2], originate_timestamp[3]]),
            receive_timestamp: u32::from_be_bytes([receive_timestamp[0], receive_timestamp[1], receive_timestamp[2], receive_timestamp[3]]),
            transmit_timestamp: u32::from_be_bytes([transmit_timestamp[0], transmit_timestamp[1], transmit_timestamp[2], transmit_timestamp[3]]),
        },
    ))
}

fn parse_icmp_info_request(input: &[u8]) -> IResult<&[u8], IcmpInfoRequest> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, unused) = take(4u8)(input)?;
    Ok((
        input,
        IcmpInfoRequest {
            header,
            unused: u32::from_be_bytes([unused[0], unused[1], unused[2], unused[3]]),
        },
    ))
}

fn parse_icmp_info_reply(input: &[u8]) -> IResult<&[u8], IcmpInfoReply> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, unused) = take(4u8)(input)?;
    Ok((
        input,
        IcmpInfoReply {
            header,
            unused: u32::from_be_bytes([unused[0], unused[1], unused[2], unused[3]]),
        },
    ))
}

fn parse_icmp_address_mask_request(input: &[u8]) -> IResult<&[u8], IcmpAddressMaskRequest> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, address) = take(4u8)(input)?;
    Ok((
        input,
        IcmpAddressMaskRequest {
            header,
            address: u32::from_be_bytes([address[0], address[1], address[2], address[3]]),
        },
    ))
}

fn parse_icmp_address_mask_reply(input: &[u8]) -> IResult<&[u8], IcmpAddressMaskReply> {
    let (input, header) = parse_icmp_header(input)?;
    let (input, address) = take(4u8)(input)?;
    Ok((
        input,
        IcmpAddressMaskReply {
            header,
            address: u32::from_be_bytes([address[0], address[1], address[2], address[3]]),
        },
    ))
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], Box<dyn std::any::Any>> {
    let (input, header) = parse_icmp_header(input)?;
    match header.icmp_type {
        IcmpType::EchoReply => {
            let (input, echo) = parse_icmp_echo(input)?;
            Ok((input, Box::new(echo)))
        }
        IcmpType::DestinationUnreachable => {
            let (input, destination_unreachable) = parse_icmp_destination_unreachable(input)?;
            Ok((input, Box::new(destination_unreachable)))
        }
        IcmpType::SourceQuench => {
            let (input, source_quench) = parse_icmp_source_quench(input)?;
            Ok((input, Box::new(source_quench)))
        }
        IcmpType::Redirect => {
            let (input, redirect) = parse_icmp_redirect(input)?;
            Ok((input, Box::new(redirect)))
        }
        IcmpType::EchoRequest => {
            let (input, echo) = parse_icmp_echo(input)?;
            Ok((input, Box::new(echo)))
        }
        IcmpType::TimeExceeded => {
            let (input, time_exceeded) = parse_icmp_time_exceeded(input)?;
            Ok((input, Box::new(time_exceeded)))
        }
        IcmpType::ParameterProblem => {
            let (input, parameter_problem) = parse_icmp_parameter_problem(input)?;
            Ok((input, Box::new(parameter_problem)))
        }
        IcmpType::Timestamp => {
            let (input, timestamp) = parse_icmp_timestamp(input)?;
            Ok((input, Box::new(timestamp)))
        }
        IcmpType::TimestampReply => {
            let (input, timestamp_reply) = parse_icmp_timestamp_reply(input)?;
            Ok((input, Box::new(timestamp_reply)))
        }
        IcmpType::InfoRequest => {
            let (input, info_request) = parse_icmp_info_request(input)?;
            Ok((input, Box::new(info_request)))
        }
        IcmpType::InfoReply => {
            let (input, info_reply) = parse_icmp_info_reply(input)?;
            Ok((input, Box::new(info_reply)))
        }
        IcmpType::AddressMaskRequest => {
            let (input, address_mask_request) = parse_icmp_address_mask_request(input)?;
            Ok((input, Box::new(address_mask_request)))
        }
        IcmpType::AddressMaskReply => {
            let (input, address_mask_reply) = parse_icmp_address_mask_reply(input)?;
            Ok((input, Box::new(address_mask_reply)))
        }
        IcmpType::Traceroute => {
            panic!("Traceroute is not implemented")
        }
    }
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }
    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    let (_rest, icmp) = parse_icmp(&buffer).unwrap();
    println!("{:?}", icmp);
    Ok(())
}