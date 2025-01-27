use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::{take},
    number::complete::{be_u8, be_u16, be_u32},
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
    TimestampRequest,
    TimestampReply,
    InfoRequest,
    InfoReply,
    AddressMaskRequest,
    AddressMaskReply,
    Unknown(u8),
}

#[derive(Debug)]
struct IcmpPacket {
    icmp_type: IcmpType,
    code: u8,
    checksum: u16,
    rest_of_header: Vec<u8>,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], IcmpType> {
    let (input, icmp_type) = be_u8(input)?;
    let icmp_type = match icmp_type {
        0 => IcmpType::EchoReply,
        3 => IcmpType::DestinationUnreachable,
        4 => IcmpType::SourceQuench,
        5 => IcmpType::Redirect,
        8 => IcmpType::EchoRequest,
        11 => IcmpType::TimeExceeded,
        12 => IcmpType::ParameterProblem,
        13 => IcmpType::TimestampRequest,
        14 => IcmpType::TimestampReply,
        15 => IcmpType::InfoRequest,
        16 => IcmpType::InfoReply,
        17 => IcmpType::AddressMaskRequest,
        18 => IcmpType::AddressMaskReply,
        _ => IcmpType::Unknown(icmp_type),
    };
    Ok((input, icmp_type))
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = take(4usize)(input)?; // Typically 4 bytes for the rest of the header
    Ok((input, IcmpPacket {
        icmp_type,
        code,
        checksum,
        rest_of_header: rest_of_header.to_vec(),
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_icmp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse ICMP packet: {:?}", e),
    }
}