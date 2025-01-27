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
enum IcmpPacket {
    EchoRequest(IcmpHeader, IcmpEcho),
    EchoReply(IcmpHeader, IcmpEcho),
    TimestampRequest(IcmpHeader, IcmpTimestamp),
    TimestampReply(IcmpHeader, IcmpTimestamp),
    AddressMaskRequest(IcmpHeader, IcmpAddressMask),
    AddressMaskReply(IcmpHeader, IcmpAddressMask),
    Other(IcmpHeader, Vec<u8>),
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

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, header) = parse_icmp_header(input)?;
    match header.type_ {
        0 | 8 => {
            let (input, echo) = parse_icmp_echo(input)?;
            if header.type_ == 8 {
                Ok((input, IcmpPacket::EchoRequest(header, echo)))
            } else {
                Ok((input, IcmpPacket::EchoReply(header, echo)))
            }
        }
        13 | 14 => {
            let (input, timestamp) = parse_icmp_timestamp(input)?;
            if header.type_ == 13 {
                Ok((input, IcmpPacket::TimestampRequest(header, timestamp)))
            } else {
                Ok((input, IcmpPacket::TimestampReply(header, timestamp)))
            }
        }
        17 | 18 => {
            let (input, address_mask) = parse_icmp_address_mask(input)?;
            if header.type_ == 17 {
                Ok((input, IcmpPacket::AddressMaskRequest(header, address_mask)))
            } else {
                Ok((input, IcmpPacket::AddressMaskReply(header, address_mask)))
            }
        }
        _ => {
            let (input, data) = take(input.len())(input)?;
            Ok((input, IcmpPacket::Other(header, data.to_vec())))
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_icmp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse ICMP packet: {:?}", e),
    }
}