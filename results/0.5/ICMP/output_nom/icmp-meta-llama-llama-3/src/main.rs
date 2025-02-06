use nom::{
    number::complete::{be_u8, be_u16},
    IResult,
};
use std::{env, fs};

#[derive(Debug, PartialEq, Clone)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    AlternateHostAddress,
    EchoRequest,
    RouterAdvertisement,
    RouterSolicitation,
    TimeExceeded,
    ParameterProblem,
    TimestampRequest,
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
    FragmentationNeededAndDontFragmentWasSet,
    SourceRouteFailed,
    DestinationNetworkUnknown,
    DestinationHostUnknown,
    SourceHostIsolated,
    NetworkAdministrativelyProhibited,
    HostAdministrativelyProhibited,
    NetworkUnreachableForTypeOfService,
    HostUnreachableForTypeOfService,
    RedirectDatagramForTheNetwork,
    RedirectDatagramForTheHost,
    RedirectDatagramForTheTypeOfServiceAndNetwork,
    RedirectDatagramForTheTypeOfServiceAndHost,
    TimeToLiveExceededInTransit,
    FragmentReassemblyTimeExceeded,
    PointerIndicatesTheError,
    MissingARequiredOption,
    BadLength,
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
    let (input, n) = be_u8(input)?;
    Ok((input, match n {
        0 => IcmpType::EchoReply,
        3 => IcmpType::DestinationUnreachable,
        4 => IcmpType::SourceQuench,
        5 => IcmpType::Redirect,
        6 => IcmpType::AlternateHostAddress,
        8 => IcmpType::EchoRequest,
        9 => IcmpType::RouterAdvertisement,
        10 => IcmpType::RouterSolicitation,
        11 => IcmpType::TimeExceeded,
        12 => IcmpType::ParameterProblem,
        13 => IcmpType::TimestampRequest,
        14 => IcmpType::TimestampReply,
        15 => IcmpType::InformationRequest,
        16 => IcmpType::InformationReply,
        n => IcmpType::Unassigned(n),
    }))
}

fn parse_icmp_code<'a>(input: &'a [u8], icmp_type: &IcmpType) -> IResult<&'a [u8], IcmpCode> {
    let (input, n) = be_u8(input)?;
    Ok((input, match icmp_type {
        IcmpType::DestinationUnreachable => match n {
            0 => IcmpCode::NetworkUnreachable,
            1 => IcmpCode::HostUnreachable,
            2 => IcmpCode::ProtocolUnreachable,
            3 => IcmpCode::PortUnreachable,
            4 => IcmpCode::FragmentationNeededAndDontFragmentWasSet,
            5 => IcmpCode::SourceRouteFailed,
            6 => IcmpCode::DestinationNetworkUnknown,
            7 => IcmpCode::DestinationHostUnknown,
            8 => IcmpCode::SourceHostIsolated,
            9 => IcmpCode::NetworkAdministrativelyProhibited,
            10 => IcmpCode::HostAdministrativelyProhibited,
            11 => IcmpCode::NetworkUnreachableForTypeOfService,
            12 => IcmpCode::HostUnreachableForTypeOfService,
            n => IcmpCode::Unassigned(n),
        },
        IcmpType::Redirect => match n {
            0 => IcmpCode::RedirectDatagramForTheNetwork,
            1 => IcmpCode::RedirectDatagramForTheHost,
            2 => IcmpCode::RedirectDatagramForTheTypeOfServiceAndNetwork,
            3 => IcmpCode::RedirectDatagramForTheTypeOfServiceAndHost,
            n => IcmpCode::Unassigned(n),
        },
        IcmpType::TimeExceeded => match n {
            0 => IcmpCode::TimeToLiveExceededInTransit,
            1 => IcmpCode::FragmentReassemblyTimeExceeded,
            n => IcmpCode::Unassigned(n),
        },
        IcmpType::ParameterProblem => match n {
            0 => IcmpCode::PointerIndicatesTheError,
            1 => IcmpCode::MissingARequiredOption,
            2 => IcmpCode::BadLength,
            n => IcmpCode::Unassigned(n),
        },
        _ => IcmpCode::Unassigned(n),
    }))
}

fn parse_icmp_header(input: &[u8]) -> IResult<&[u8], IcmpHeader> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = parse_icmp_code(input, &icmp_type)?;
    let (input, checksum) = be_u16(input)?;
    let (input, identifier) = be_u16(input)?;
    let (input, sequence_number) = be_u16(input)?;
    Ok((
        input,
        IcmpHeader {
            icmp_type,
            code,
            checksum,
            identifier,
            sequence_number,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }
    let file_name = &args[1];
    let data = match fs::read(file_name) {
        Ok(data) => data,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };
    match parse_icmp_header(&data) {
        Ok((_, header)) => println!("{:?}", header),
        Err(err) => eprintln!("Error parsing ICMP header: {:?}", err),
    }
}