use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

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
        tuple((be_u8, be_u8, be_u16, take(4usize)))(input)?;
    let rest_of_header = u32::from_be_bytes(rest_of_header.try_into().unwrap());
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
        tuple((take(4usize), take(4usize), take(4usize)))(input)?;
    let originate_timestamp = u32::from_be_bytes(originate_timestamp.try_into().unwrap());
    let receive_timestamp = u32::from_be_bytes(receive_timestamp.try_into().unwrap());
    let transmit_timestamp = u32::from_be_bytes(transmit_timestamp.try_into().unwrap());
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
    let (input, address_mask) = take(4usize)(input)?;
    let address_mask = u32::from_be_bytes(address_mask.try_into().unwrap());
    Ok((input, IcmpAddressMask { address_mask }))
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, header) = parse_icmp_header(input)?;
    match header.type_ {
        8 => {
            let (input, echo) = parse_icmp_echo(input)?;
            Ok((input, IcmpPacket::EchoRequest(header, echo)))
        }
        0 => {
            let (input, echo) = parse_icmp_echo(input)?;
            Ok((input, IcmpPacket::EchoReply(header, echo)))
        }
        13 => {
            let (input, timestamp) = parse_icmp_timestamp(input)?;
            Ok((input, IcmpPacket::TimestampRequest(header, timestamp)))
        }
        14 => {
            let (input, timestamp) = parse_icmp_timestamp(input)?;
            Ok((input, IcmpPacket::TimestampReply(header, timestamp)))
        }
        17 => {
            let (input, address_mask) = parse_icmp_address_mask(input)?;
            Ok((input, IcmpPacket::AddressMaskRequest(header, address_mask)))
        }
        18 => {
            let (input, address_mask) = parse_icmp_address_mask(input)?;
            Ok((input, IcmpPacket::AddressMaskReply(header, address_mask)))
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
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let mut file = fs::File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_icmp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse ICMP packet: {:?}", e),
    }
}