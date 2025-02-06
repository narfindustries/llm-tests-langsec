use nom::{
    bits::{streaming::take as take_bits, complete::take as take_bits_complete},
    bytes::complete::take,
    combinator::{map, map_opt},
    error::ErrorKind,
    multi::many0,
    number::complete::{be_u8, be_u16, be_u32},
    sequence::tuple,
    IResult, Err,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum IcmpType {
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

#[derive(Debug, PartialEq)]
enum DestUnreachableCode {
    NetUnreachable,
    HostUnreachable,
    ProtocolUnreachable,
    PortUnreachable,
    FragmentationNeeded,
    SourceRouteFailed,
    DestNetUnknown,
    DestHostUnknown,
    Unknown(u8),
}

#[derive(Debug, PartialEq)]
enum RedirectCode {
    NetworkRedirect,
    HostRedirect,
    TosNetRedirect,
    TosHostRedirect,
    Unknown(u8),
}

#[derive(Debug, PartialEq)]
enum TimeExceededCode {
    TTLExpired,
    FragmentReassemblyTimeExceeded,
    Unknown(u8),
}

#[derive(Debug, PartialEq)]
enum ParameterProblemCode {
    PointerIndicatesError,
    MissingRequiredOption,
    BadLength,
    Unknown(u8),
}

#[derive(Debug)]
struct IcmpPacket {
    icmp_type: IcmpType,
    code: u8,
    checksum: u16,
    rest: Option<Vec<u8>>,
}

fn parse_icmp_type(input: u8) -> IcmpType {
    match input {
        0 => IcmpType::EchoReply,
        3 => IcmpType::DestinationUnreachable(parse_dest_unreachable_code(input)),
        4 => IcmpType::SourceQuench,
        5 => IcmpType::Redirect(parse_redirect_code(input)),
        8 => IcmpType::EchoRequest,
        9 => IcmpType::RouterAdvertisement,
        10 => IcmpType::RouterSolicitation,
        11 => IcmpType::TimeExceeded(parse_time_exceeded_code(input)),
        12 => IcmpType::ParameterProblem(parse_parameter_problem_code(input)),
        13 => IcmpType::Timestamp,
        14 => IcmpType::TimestampReply,
        15 => IcmpType::InformationRequest,
        16 => IcmpType::InformationReply,
        _ => IcmpType::Unknown(input),
    }
}

fn parse_dest_unreachable_code(code: u8) -> DestUnreachableCode {
    match code {
        0 => DestUnreachableCode::NetUnreachable,
        1 => DestUnreachableCode::HostUnreachable,
        2 => DestUnreachableCode::ProtocolUnreachable,
        3 => DestUnreachableCode::PortUnreachable,
        4 => DestUnreachableCode::FragmentationNeeded,
        5 => DestUnreachableCode::SourceRouteFailed,
        6 => DestUnreachableCode::DestNetUnknown,
        7 => DestUnreachableCode::DestHostUnknown,
        _ => DestUnreachableCode::Unknown(code),
    }
}

fn parse_redirect_code(code: u8) -> RedirectCode {
    match code {
        0 => RedirectCode::NetworkRedirect,
        1 => RedirectCode::HostRedirect,
        2 => RedirectCode::TosNetRedirect,
        3 => RedirectCode::TosHostRedirect,
        _ => RedirectCode::Unknown(code),
    }
}

fn parse_time_exceeded_code(code: u8) -> TimeExceededCode {
    match code {
        0 => TimeExceededCode::TTLExpired,
        1 => TimeExceededCode::FragmentReassemblyTimeExceeded,
        _ => TimeExceededCode::Unknown(code),
    }
}

fn parse_parameter_problem_code(code: u8) -> ParameterProblemCode {
    match code {
        0 => ParameterProblemCode::PointerIndicatesError,
        1 => ParameterProblemCode::MissingRequiredOption,
        2 => ParameterProblemCode::BadLength,
        _ => ParameterProblemCode::Unknown(code),
    }
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, (raw_type, code, checksum)) = tuple((be_u8, be_u8, be_u16))(input)?;
    
    let icmp_type = parse_icmp_type(raw_type);
    
    let (input, rest) = match icmp_type {
        IcmpType::EchoReply | IcmpType::EchoRequest => {
            let (input, rest) = take(4usize)(input)?;
            (input, Some(rest.to_vec()))
        },
        _ => (input, None)
    };

    Ok((input, IcmpPacket {
        icmp_type,
        code,
        checksum,
        rest,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_icmp(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed ICMP Packet: {:?}", packet);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}