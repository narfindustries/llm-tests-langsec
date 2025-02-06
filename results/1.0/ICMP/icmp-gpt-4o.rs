use std::fs::File;
use std::io::{self, Read};
use std::env;
use nom::{
    IResult,
    bytes::complete::{take},
    number::complete::{be_u8, be_u16},
};

#[derive(Debug)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable(u8),
    SourceQuench,
    Redirect(u8),
    EchoRequest,
    RouterAdvertisement,
    RouterSolicitation,
    TimeExceeded(u8),
    ParameterProblem(u8),
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
    checksum: u16,
    rest_of_header: Vec<u8>,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], IcmpType> {
    let (input, icmp_type) = be_u8(input)?;
    let (input, code) = be_u8(input)?;
    let icmp_type = match icmp_type {
        0 => IcmpType::EchoReply,
        3 => IcmpType::DestinationUnreachable(code),
        4 => IcmpType::SourceQuench,
        5 => IcmpType::Redirect(code),
        8 => IcmpType::EchoRequest,
        9 => IcmpType::RouterAdvertisement,
        10 => IcmpType::RouterSolicitation,
        11 => IcmpType::TimeExceeded(code),
        12 => IcmpType::ParameterProblem(code),
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

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = take(input.len())(input)?;
    Ok((input, IcmpPacket {
        icmp_type,
        checksum,
        rest_of_header: rest_of_header.to_vec(),
    }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    
    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buf = Vec::new();
    file.read_to_end(&mut buf)?;

    match parse_icmp_packet(&buf) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse ICMP packet: {:?}", e),
    }

    Ok(())
}