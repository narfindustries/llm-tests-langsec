use nom::{
    bytes::complete::take,
    combinator::map_res,
    error::{Error, ErrorKind},
    number::complete::be_u16,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq, Clone)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    AlternateHostAddress,
    Echo,
    RouterAdvertisement,
    RouterSolicitation,
    TimeExceeded,
    ParameterProblem,
    Timestamp,
    TimestampReply,
    InformationRequest,
    InformationReply,
    Unassigned(u8),
}

#[derive(Debug, PartialEq, Clone)]
enum IcmpCode {
    NetworkUnreachable,
    HostUnreachable,
    ProtocolUnreachable,
    PortUnreachable,
    FragmentationNeeded,
    SourceRouteFailed,
    RedirectForNetwork,
    RedirectForHost,
    RedirectForTypeOfServiceAndNetwork,
    RedirectForTypeOfServiceAndHost,
    TimeToLiveExceeded,
    FragmentReassemblyTimeExceeded,
    PointerIndicatesError,
    MissingRequiredOption,
    Unassigned(u8),
}

#[derive(Debug, PartialEq)]
struct IcmpHeader {
    icmp_type: IcmpType,
    code: IcmpCode,
    checksum: u16,
    identifier: u16,
    sequence_number: u16,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], IcmpType> {
    map_res(take(1usize), |x: &[u8]| match x[0] {
        0 => Ok(IcmpType::EchoReply),
        3 => Ok(IcmpType::DestinationUnreachable),
        4 => Ok(IcmpType::SourceQuench),
        5 => Ok(IcmpType::Redirect),
        6 => Ok(IcmpType::AlternateHostAddress),
        8 => Ok(IcmpType::Echo),
        9 => Ok(IcmpType::RouterAdvertisement),
        10 => Ok(IcmpType::RouterSolicitation),
        11 => Ok(IcmpType::TimeExceeded),
        12 => Ok(IcmpType::ParameterProblem),
        13 => Ok(IcmpType::Timestamp),
        14 => Ok(IcmpType::TimestampReply),
        15 => Ok(IcmpType::InformationRequest),
        16 => Ok(IcmpType::InformationReply),
        x => Ok(IcmpType::Unassigned(x)),
    })(input)
}

fn parse_icmp_code<'a>(input: &'a [u8], icmp_type: &IcmpType) -> IResult<&'a [u8], IcmpCode> {
    map_res(take(1usize), |x: &[u8]| match icmp_type {
        IcmpType::DestinationUnreachable => match x[0] {
            0 => Ok(IcmpCode::NetworkUnreachable),
            1 => Ok(IcmpCode::HostUnreachable),
            2 => Ok(IcmpCode::ProtocolUnreachable),
            3 => Ok(IcmpCode::PortUnreachable),
            4 => Ok(IcmpCode::FragmentationNeeded),
            5 => Ok(IcmpCode::SourceRouteFailed),
            x => Ok(IcmpCode::Unassigned(x)),
        },
        IcmpType::Redirect => match x[0] {
            0 => Ok(IcmpCode::RedirectForNetwork),
            1 => Ok(IcmpCode::RedirectForHost),
            2 => Ok(IcmpCode::RedirectForTypeOfServiceAndNetwork),
            3 => Ok(IcmpCode::RedirectForTypeOfServiceAndHost),
            x => Ok(IcmpCode::Unassigned(x)),
        },
        IcmpType::TimeExceeded => match x[0] {
            0 => Ok(IcmpCode::TimeToLiveExceeded),
            1 => Ok(IcmpCode::FragmentReassemblyTimeExceeded),
            x => Ok(IcmpCode::Unassigned(x)),
        },
        IcmpType::ParameterProblem => match x[0] {
            0 => Ok(IcmpCode::PointerIndicatesError),
            1 => Ok(IcmpCode::MissingRequiredOption),
            x => Ok(IcmpCode::Unassigned(x)),
        },
        _ => Ok(IcmpCode::Unassigned(x[0])),
    })(input)
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = parse_icmp_code(input, &icmp_type)?;
    let (input, checksum) = be_u16(input)?;
    let (input, identifier) = be_u16(input)?;
    let (input, sequence_number) = be_u16(input)?;
    Ok((input, IcmpHeader { icmp_type, code, checksum, identifier, sequence_number }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read file");
    match parse_icmp_header(&data) {
        Ok((_, header)) => println!("{:?}", header),
        Err(err) => println!("Error: {:?}", err),
    }
}