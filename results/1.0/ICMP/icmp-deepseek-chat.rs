use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::Read,
};

#[derive(Debug)]
struct IcmpHeader {
    type_: u8,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
}

#[derive(Debug)]
struct IcmpEcho {
    identifier: u16,
    sequence_number: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct IcmpTimestamp {
    originate_timestamp: u32,
    receive_timestamp: u32,
    transmit_timestamp: u32,
}

#[derive(Debug)]
struct IcmpAddressMask {
    address_mask: u32,
}

#[derive(Debug)]
enum IcmpMessage {
    EchoRequest(IcmpEcho),
    EchoReply(IcmpEcho),
    TimestampRequest(IcmpTimestamp),
    TimestampReply(IcmpTimestamp),
    AddressMaskRequest(IcmpAddressMask),
    AddressMaskReply(IcmpAddressMask),
    Other(IcmpHeader),
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, (type_, code, checksum, rest_of_header)) =
        tuple((be_u8, be_u8, be_u16, be_u32))(input)?;
    Ok((
        input,
        IcmpHeader {
            type_,
            code,
            checksum,
            rest_of_header,
        },
    ))
}

fn parse_icmp_echo(input: &[u8]) -> IResult<&[u8], IcmpEcho> {
    let (input, (identifier, sequence_number)) = tuple((be_u16, be_u16))(input)?;
    let (input, data) = take(input.len())(input)?;
    Ok((
        input,
        IcmpEcho {
            identifier,
            sequence_number,
            data: data.to_vec(),
        },
    ))
}

fn parse_icmp_timestamp(input: &[u8]) -> IResult<&[u8], IcmpTimestamp> {
    let (input, (originate_timestamp, receive_timestamp, transmit_timestamp)) =
        tuple((be_u32, be_u32, be_u32))(input)?;
    Ok((
        input,
        IcmpTimestamp {
            originate_timestamp,
            receive_timestamp,
            transmit_timestamp,
        },
    ))
}

fn parse_icmp_address_mask(input: &[u8]) -> IResult<&[u8], IcmpAddressMask> {
    let (input, address_mask) = be_u32(input)?;
    Ok((input, IcmpAddressMask { address_mask }))
}

fn parse_icmp_message(input: &[u8]) -> IResult<&[u8], IcmpMessage> {
    let (input, header) = parse_icmp_header(input)?;
    match header.type_ {
        0 => {
            let (input, echo) = parse_icmp_echo(input)?;
            Ok((input, IcmpMessage::EchoReply(echo)))
        }
        8 => {
            let (input, echo) = parse_icmp_echo(input)?;
            Ok((input, IcmpMessage::EchoRequest(echo)))
        }
        13 => {
            let (input, timestamp) = parse_icmp_timestamp(input)?;
            Ok((input, IcmpMessage::TimestampRequest(timestamp)))
        }
        14 => {
            let (input, timestamp) = parse_icmp_timestamp(input)?;
            Ok((input, IcmpMessage::TimestampReply(timestamp)))
        }
        17 => {
            let (input, address_mask) = parse_icmp_address_mask(input)?;
            Ok((input, IcmpMessage::AddressMaskRequest(address_mask)))
        }
        18 => {
            let (input, address_mask) = parse_icmp_address_mask(input)?;
            Ok((input, IcmpMessage::AddressMaskReply(address_mask)))
        }
        _ => Ok((input, IcmpMessage::Other(header))),
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
        Ok((_, message)) => println!("{:#?}", message),
        Err(e) => eprintln!("Failed to parse ICMP message: {:?}", e),
    }
}