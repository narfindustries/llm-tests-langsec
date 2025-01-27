use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct IcmpHeader {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
}

#[derive(Debug)]
struct IcmpEchoRequest {
    identifier: u16,
    sequence_number: u16,
}

#[derive(Debug)]
struct IcmpRedirect {
    gateway_internet_address: u32,
}

#[derive(Debug)]
enum IcmpMessage {
    EchoReply(IcmpHeader, Vec<u8>),
    DestinationUnreachable(IcmpHeader, Vec<u8>),
    SourceQuench(IcmpHeader, Vec<u8>),
    Redirect(IcmpHeader, IcmpRedirect, Vec<u8>),
    EchoRequest(IcmpHeader, IcmpEchoRequest, Vec<u8>),
    TimeExceeded(IcmpHeader, Vec<u8>),
    ParameterProblem(IcmpHeader, Vec<u8>),
    TimestampRequest(IcmpHeader),
    TimestampReply(IcmpHeader),
    InformationRequest(IcmpHeader),
    InformationReply(IcmpHeader),
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    map(
        tuple((be_u8, be_u8, be_u16, be_u32)),
        |(icmp_type, code, checksum, rest_of_header)| IcmpHeader {
            icmp_type,
            code,
            checksum,
            rest_of_header,
        },
    )(input)
}

fn parse_echo_request(input: &[u8]) -> IResult<&[u8], IcmpEchoRequest> {
    map(
        tuple((be_u16, be_u16)),
        |(identifier, sequence_number)| IcmpEchoRequest {
            identifier,
            sequence_number,
        },
    )(input)
}

fn parse_redirect(input: &[u8]) -> IResult<&[u8], IcmpRedirect> {
    map(be_u32, |gateway_internet_address| IcmpRedirect {
        gateway_internet_address,
    })(input)
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpMessage> {
    let (remaining, header) = parse_icmp_header(input)?;

    match header.icmp_type {
        0 => {
            let (remaining, data) = take(remaining.len())(remaining)?;
            Ok((remaining, IcmpMessage::EchoReply(header, data.to_vec())))
        }
        3 => {
            let (remaining, data) = take(remaining.len())(remaining)?;
            Ok((
                remaining,
                IcmpMessage::DestinationUnreachable(header, data.to_vec()),
            ))
        }
        4 => {
            let (remaining, data) = take(remaining.len())(remaining)?;
            Ok((remaining, IcmpMessage::SourceQuench(header, data.to_vec())))
        }
        5 => {
            let (remaining, redirect) = parse_redirect(remaining)?;
            let (remaining, data) = take(remaining.len())(remaining)?;
            Ok((
                remaining,
                IcmpMessage::Redirect(header, redirect, data.to_vec()),
            ))
        }
        8 => {
            let (remaining, echo_request) = parse_echo_request(remaining)?;
            let (remaining, data) = take(remaining.len())(remaining)?;
            Ok((
                remaining,
                IcmpMessage::EchoRequest(header, echo_request, data.to_vec()),
            ))
        }
        11 => {
            let (remaining, data) = take(remaining.len())(remaining)?;
            Ok((remaining, IcmpMessage::TimeExceeded(header, data.to_vec())))
        }
        12 => {
            let (remaining, data) = take(remaining.len())(remaining)?;
            Ok((
                remaining,
                IcmpMessage::ParameterProblem(header, data.to_vec()),
            ))
        }
        13 => Ok((remaining, IcmpMessage::TimestampRequest(header))),
        14 => Ok((remaining, IcmpMessage::TimestampReply(header))),
        15 => Ok((remaining, IcmpMessage::InformationRequest(header))),
        16 => Ok((remaining, IcmpMessage::InformationReply(header))),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_icmp(&buffer) {
        Ok((remaining, icmp_message)) => {
            println!("Parsed ICMP message: {:?}", icmp_message);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse ICMP message: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}