use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    number::complete::{be_u8, be_u16, be_u32},
    bytes::complete::take,
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
    AddressMaskRequest,
    AddressMaskReply,
    Unknown(u8),
}

#[derive(Debug)]
struct IcmpPacket {
    icmp_type: IcmpType,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
    data: Vec<u8>,
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
        13 => IcmpType::Timestamp,
        14 => IcmpType::TimestampReply,
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
    let (input, rest_of_header) = be_u32(input)?;
    let (input, data) = take(input.len())(input)?;

    Ok((
        input,
        IcmpPacket {
            icmp_type,
            code,
            checksum,
            rest_of_header,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
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