use nom::{
    bytes::complete::take,
    multi::many0,
    number::complete::{be_u8, be_u16},
    IResult,
    combinator::{map, opt},
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
enum ICMPCode {
    DestUnreachable(DestUnreachableCode),
    TimeExceeded(TimeExceededCode),
    Redirect(RedirectCode),
    Generic(u8),
}

#[derive(Debug, PartialEq, Clone)]
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

#[derive(Debug, PartialEq, Clone)]
enum TimeExceededCode {
    TTLExpired,
    FragmentReassemblyTimeout,
    Unknown(u8),
}

#[derive(Debug, PartialEq, Clone)]
enum RedirectCode {
    NetworkRedirect,
    HostRedirect,
    TypeOfServiceNetRedirect,
    TypeOfServiceHostRedirect,
    Unknown(u8),
}

#[derive(Debug)]
struct ICMPHeader {
    icmp_type: ICMPType,
    code: ICMPCode,
    checksum: u16,
    rest_of_header: Option<Vec<u8>>,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], ICMPType> {
    map(be_u8, |val| match val {
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
        x => ICMPType::Unknown(x),
    })(input)
}

fn parse_icmp_code(icmp_type: ICMPType, input: &[u8]) -> IResult<&[u8], ICMPCode> {
    map(be_u8, |val| match icmp_type {
        ICMPType::DestinationUnreachable => ICMPCode::DestUnreachable(match val {
            0 => DestUnreachableCode::NetUnreachable,
            1 => DestUnreachableCode::HostUnreachable,
            2 => DestUnreachableCode::ProtocolUnreachable,
            3 => DestUnreachableCode::PortUnreachable,
            4 => DestUnreachableCode::FragmentationNeeded,
            5 => DestUnreachableCode::SourceRouteFailed,
            6 => DestUnreachableCode::DestNetUnknown,
            7 => DestUnreachableCode::DestHostUnknown,
            x => DestUnreachableCode::Unknown(x),
        }),
        ICMPType::TimeExceeded => ICMPCode::TimeExceeded(match val {
            0 => TimeExceededCode::TTLExpired,
            1 => TimeExceededCode::FragmentReassemblyTimeout,
            x => TimeExceededCode::Unknown(x),
        }),
        ICMPType::Redirect => ICMPCode::Redirect(match val {
            0 => RedirectCode::NetworkRedirect,
            1 => RedirectCode::HostRedirect,
            2 => RedirectCode::TypeOfServiceNetRedirect,
            3 => RedirectCode::TypeOfServiceHostRedirect,
            x => RedirectCode::Unknown(x),
        }),
        _ => ICMPCode::Generic(val),
    })(input)
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], ICMPHeader> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = parse_icmp_code(icmp_type.clone(), input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = opt(take(4usize))(input)?;

    Ok((input, ICMPHeader {
        icmp_type,
        code,
        checksum,
        rest_of_header: rest_of_header.map(|x| x.to_vec()),
    }))
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], Vec<ICMPHeader>> {
    many0(parse_icmp_header)(input)
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
        Ok((_, headers)) => {
            for header in headers {
                println!("{:?}", header);
            }
        }
        Err(e) => eprintln!("Parsing error: {:?}", e),
    }

    Ok(())
}