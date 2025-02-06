use nom::{
    bits::{streaming::take as take_bits, complete::take as take_bits_complete},
    bytes::complete::{take},
    error::Error,
    multi::many0,
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple},
    IResult,
    Parser,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum ICMPType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest,
    RouterAdvertisement,
    RouterSolicitation,
    TimeExceeded,
    ParameterProblem,
    Timestamp,
    TimestampReply,
    InformationRequest,
    InformationReply,
    Unknown(u8),
}

#[derive(Debug)]
struct ICMPPacket {
    icmp_type: ICMPType,
    code: u8,
    checksum: u16,
    rest_of_header: Vec<u8>,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], ICMPType> {
    be_u8.map(|val| match val {
        0 => ICMPType::EchoReply,
        3 => ICMPType::DestinationUnreachable,
        4 => ICMPType::SourceQuench,
        5 => ICMPType::Redirect,
        8 => ICMPType::EchoRequest,
        9 => ICMPType::RouterAdvertisement,
        10 => ICMPType::RouterSolicitation,
        11 => ICMPType::TimeExceeded,
        12 => ICMPType::ParameterProblem,
        13 => ICMPType::Timestamp,
        14 => ICMPType::TimestampReply,
        15 => ICMPType::InformationRequest,
        16 => ICMPType::InformationReply,
        x => ICMPType::Unknown(x)
    }).parse(input)
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], ICMPPacket> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = take(4usize)(input)?;

    Ok((input, ICMPPacket {
        icmp_type,
        code,
        checksum,
        rest_of_header: rest_of_header.to_vec(),
    }))
}

fn parse_icmp_packets(input: &[u8]) -> IResult<&[u8], Vec<ICMPPacket>> {
    many0(parse_icmp_packet)(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_icmp_packets(&buffer) {
        Ok((_, packets)) => {
            for packet in packets {
                println!("ICMP Packet: {:?}", packet);
            }
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}