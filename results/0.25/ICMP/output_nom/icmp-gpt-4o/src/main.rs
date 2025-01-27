use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    number::complete::{be_u8, be_u16, be_u32},
    bytes::complete::take,
};

#[derive(Debug)]
enum ICMPType {
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
struct ICMPPacket {
    icmp_type: ICMPType,
    code: u8,
    checksum: u16,
    rest_of_header: Vec<u8>,
    data: Vec<u8>,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], ICMPType> {
    let (input, icmp_type) = be_u8(input)?;
    let icmp_type = match icmp_type {
        0 => ICMPType::EchoReply,
        3 => ICMPType::DestinationUnreachable,
        4 => ICMPType::SourceQuench,
        5 => ICMPType::Redirect,
        8 => ICMPType::EchoRequest,
        11 => ICMPType::TimeExceeded,
        12 => ICMPType::ParameterProblem,
        13 => ICMPType::TimestampRequest,
        14 => ICMPType::TimestampReply,
        15 => ICMPType::InfoRequest,
        16 => ICMPType::InfoReply,
        17 => ICMPType::AddressMaskRequest,
        18 => ICMPType::AddressMaskReply,
        _ => ICMPType::Unknown(icmp_type),
    };
    Ok((input, icmp_type))
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], ICMPPacket> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = take(4usize)(input)?;
    let (input, data) = take(input.len())(input)?;

    Ok((input, ICMPPacket {
        icmp_type,
        code,
        checksum,
        rest_of_header: rest_of_header.to_vec(),
        data: data.to_vec(),
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
        Ok((_, packet)) => {
            println!("{:?}", packet);
        }
        Err(e) => {
            eprintln!("Failed to parse ICMP packet: {:?}", e);
        }
    }
}