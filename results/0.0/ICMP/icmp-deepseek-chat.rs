use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
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
enum IcmpPacket {
    EchoRequest(IcmpEcho),
    EchoReply(IcmpEcho),
    Other(IcmpHeader, Vec<u8>),
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, (type_, code, checksum, rest_of_header)) =
        tuple((be_u8, be_u8, be_u16, take(4usize)))(input)?;
    let rest_of_header = u32::from_be_bytes(rest_of_header.try_into().unwrap());
    Ok((input, IcmpHeader {
        type_,
        code,
        checksum,
        rest_of_header,
    }))
}

fn parse_icmp_echo(input: &[u8]) -> IResult<&[u8], IcmpEcho> {
    let (input, (identifier, sequence_number)) = tuple((be_u16, be_u16))(input)?;
    let (input, data) = take(input.len())(input)?;
    Ok((input, IcmpEcho {
        identifier,
        sequence_number,
        data: data.to_vec(),
    }))
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, header) = parse_icmp_header(input)?;
    match (header.type_, header.code) {
        (8, 0) => {
            let (input, echo) = parse_icmp_echo(input)?;
            Ok((input, IcmpPacket::EchoRequest(echo)))
        }
        (0, 0) => {
            let (input, echo) = parse_icmp_echo(input)?;
            Ok((input, IcmpPacket::EchoReply(echo)))
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