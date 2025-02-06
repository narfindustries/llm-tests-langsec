use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct IcmpHeader {
    type_field: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
}

#[derive(Debug)]
struct IcmpEcho {
    identifier: u16,
    sequence_number: u16,
}

#[derive(Debug)]
struct IcmpTimestamp {
    originate_timestamp: u32,
    receive_timestamp: u32,
    transmit_timestamp: u32,
}

#[derive(Debug)]
enum IcmpMessage {
    EchoRequest(IcmpHeader, IcmpEcho),
    EchoReply(IcmpHeader, IcmpEcho),
    DestinationUnreachable(IcmpHeader),
    Redirect(IcmpHeader, u32),
    TimeExceeded(IcmpHeader),
    ParameterProblem(IcmpHeader),
    TimestampRequest(IcmpHeader, IcmpTimestamp),
    TimestampReply(IcmpHeader, IcmpTimestamp),
    Other(IcmpHeader, Vec<u8>),
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, type_field) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = be_u32(input)?;
    Ok((
        input,
        IcmpHeader {
            type_field,
            code,
            checksum,
            rest_of_header,
        },
    ))
}

fn parse_icmp_echo(input: &[u8]) -> IResult<&[u8], IcmpEcho> {
    let (input, identifier) = be_u16(input)?;
    let (input, sequence_number) = be_u16(input)?;
    Ok((input, IcmpEcho { identifier, sequence_number }))
}

fn parse_icmp_timestamp(input: &[u8]) -> IResult<&[u8], IcmpTimestamp> {
    let (input, originate_timestamp) = be_u32(input)?;
    let (input, receive_timestamp) = be_u32(input)?;
    let (input, transmit_timestamp) = be_u32(input)?;
    Ok((
        input,
        IcmpTimestamp {
            originate_timestamp,
            receive_timestamp,
            transmit_timestamp,
        },
    ))
}

fn parse_icmp_message(input: &[u8]) -> IResult<&[u8], IcmpMessage> {
    let (input, header) = parse_icmp_header(input)?;
    match header.type_field {
        0 | 8 => {
            let (input, echo) = parse_icmp_echo(input)?;
            if header.type_field == 0 {
                Ok((input, IcmpMessage::EchoReply(header, echo)))
            } else {
                Ok((input, IcmpMessage::EchoRequest(header, echo)))
            }
        }
        3 => Ok((input, IcmpMessage::DestinationUnreachable(header))),
        5 => {
            let gateway_address = header.rest_of_header;
            Ok((input, IcmpMessage::Redirect(header, gateway_address)))
        }
        11 => Ok((input, IcmpMessage::TimeExceeded(header))),
        12 => Ok((input, IcmpMessage::ParameterProblem(header))),
        13 | 14 => {
            let (input, timestamp) = parse_icmp_timestamp(input)?;
            if header.type_field == 13 {
                Ok((input, IcmpMessage::TimestampRequest(header, timestamp)))
            } else {
                Ok((input, IcmpMessage::TimestampReply(header, timestamp)))
            }
        }
        _ => {
            let (input, data) = take(input.len())(input)?;
            Ok((input, IcmpMessage::Other(header, data.to_vec())))
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_icmp_message(&buffer) {
        Ok((_, message)) => println!("{:?}", message),
        Err(e) => eprintln!("Failed to parse ICMP message: {:?}", e),
    }
}