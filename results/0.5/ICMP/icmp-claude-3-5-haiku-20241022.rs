use nom::{
    bits::{complete::take as take_bits, streaming::take as take_bits_streaming},
    bytes::complete::{take, tag},
    combinator::{map, complete},
    error::ErrorKind,
    multi::many0,
    number::complete::{be_u8, be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum ICMPType {
    EchoReply,
    DestinationUnreachable(DestUnreachableCode),
    SourceQuench,
    Redirect(RedirectCode),
    EchoRequest,
    RouterAdvertisement,
    RouterSolicitation,
    TimeExceeded(TimeExceededCode),
    ParameterProblem(ParameterProblemCode),
    Timestamp,
    TimestampReply,
    InformationRequest,
    InformationReply,
    Unknown(u8),
}

#[derive(Debug)]
enum DestUnreachableCode {
    NetUnreachable,
    HostUnreachable,
    ProtocolUnreachable,
    PortUnreachable,
    FragmentationNeeded,
    SourceRouteFailed,
    NetworkUnknown,
    HostUnknown,
    Unknown(u8),
}

#[derive(Debug)]
enum RedirectCode {
    NetworkRedirect,
    HostRedirect,
    NetworkTypeOfServiceRedirect,
    HostTypeOfServiceRedirect,
    Unknown(u8),
}

#[derive(Debug)]
enum TimeExceededCode {
    TTLExpired,
    FragmentReassemblyTimeExceeded,
    Unknown(u8),
}

#[derive(Debug)]
enum ParameterProblemCode {
    PointerIndicatesError,
    MissingRequiredOption,
    BadLength,
    Unknown(u8),
}

#[derive(Debug)]
struct ICMPHeader {
    icmp_type: ICMPType,
    code: u8,
    checksum: u16,
    rest_of_header: u32,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], ICMPType> {
    map(be_u8, |type_val| match type_val {
        0 => ICMPType::EchoReply,
        3 => ICMPType::DestinationUnreachable(match input[1] {
            0 => DestUnreachableCode::NetUnreachable,
            1 => DestUnreachableCode::HostUnreachable,
            2 => DestUnreachableCode::ProtocolUnreachable,
            3 => DestUnreachableCode::PortUnreachable,
            4 => DestUnreachableCode::FragmentationNeeded,
            5 => DestUnreachableCode::SourceRouteFailed,
            6 => DestUnreachableCode::NetworkUnknown,
            7 => DestUnreachableCode::HostUnknown,
            x => DestUnreachableCode::Unknown(x),
        }),
        4 => ICMPType::SourceQuench,
        5 => ICMPType::Redirect(match input[1] {
            0 => RedirectCode::NetworkRedirect,
            1 => RedirectCode::HostRedirect,
            2 => RedirectCode::NetworkTypeOfServiceRedirect,
            3 => RedirectCode::HostTypeOfServiceRedirect,
            x => RedirectCode::Unknown(x),
        }),
        8 => ICMPType::EchoRequest,
        9 => ICMPType::RouterAdvertisement,
        10 => ICMPType::RouterSolicitation,
        11 => ICMPType::TimeExceeded(match input[1] {
            0 => TimeExceededCode::TTLExpired,
            1 => TimeExceededCode::FragmentReassemblyTimeExceeded,
            x => TimeExceededCode::Unknown(x),
        }),
        12 => ICMPType::ParameterProblem(match input[1] {
            0 => ParameterProblemCode::PointerIndicatesError,
            1 => ParameterProblemCode::MissingRequiredOption,
            2 => ParameterProblemCode::BadLength,
            x => ParameterProblemCode::Unknown(x),
        }),
        13 => ICMPType::Timestamp,
        14 => ICMPType::TimestampReply,
        15 => ICMPType::InformationRequest,
        16 => ICMPType::InformationReply,
        x => ICMPType::Unknown(x),
    })(input)
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], ICMPHeader> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = be_u8(input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = be_u32(input)?;

    Ok((input, ICMPHeader {
        icmp_type,
        code,
        checksum,
        rest_of_header,
    }))
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], ICMPHeader> {
    complete(parse_icmp_header)(input)
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

    match parse_icmp_packet(&buffer) {
        Ok((_, header)) => {
            println!("Parsed ICMP Header: {:?}", header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing ICMP packet: {:?}", e);
            std::process::exit(1);
        }
    }
}