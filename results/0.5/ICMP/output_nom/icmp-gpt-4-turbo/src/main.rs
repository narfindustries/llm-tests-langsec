use nom::{
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    RedirectMessage,
    EchoRequest,
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
        5 => IcmpType::RedirectMessage,
        8 => IcmpType::EchoRequest,
        11 => IcmpType::TimeExceeded,
        12 => IcmpType::ParameterProblem,
        13 => IcmpType::Timestamp,
        14 => IcmpType::TimestampReply,
        15 => IcmpType::InformationRequest,
        16 => IcmpType::InformationReply,
        17 => IcmpType::AddressMaskRequest,
        18 => IcmpType::AddressMaskReply,
        t => IcmpType::Unknown(t),
    };
    Ok((input, icmp_type))
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = nom::bytes::complete::take(4usize)(input)?;

    Ok((
        input,
        IcmpPacket {
            icmp_type,
            code,
            checksum,
            rest_of_header: rest_of_header.to_vec(),
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_icmp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => println!("Failed to parse: {:?}", e),
    }

    Ok(())
}