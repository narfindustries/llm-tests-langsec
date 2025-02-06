use nom::{
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    Echo,
    TimeExceeded,
    ParameterProblem,
    Timestamp,
    TimestampReply,
    InformationRequest,
    InformationReply,
    AddressMaskRequest,
    AddressMaskReply,
    Unknown(u8),
}

#[derive(Debug)]
struct IcmpHeader {
    icmp_type: IcmpType,
    code: u8,
    checksum: u16,
}

#[derive(Debug)]
struct IcmpPacket {
    header: IcmpHeader,
    data: Vec<u8>,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], IcmpType> {
    let (input, icmp_type) = be_u8(input)?;
    let icmp_type = match icmp_type {
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
        _ => IcmpType::Unknown(icmp_type),
    };
    Ok((input, icmp_type))
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    Ok((input, IcmpHeader { icmp_type, code, checksum }))
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, header) = parse_icmp_header(input)?;
    let (remaining, data) = nom::bytes::complete::take(input.len())(input)?;
    Ok((remaining, IcmpPacket { header, data: data.to_vec() }))
}

fn read_file_contents(filename: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(filename)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    Ok(data)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <ICMP_PACKET_FILE>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let data = read_file_contents(filename)?;

    match parse_icmp_packet(&data) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => println!("Failed to parse ICMP packet: {:?}", e),
    }

    Ok(())
}