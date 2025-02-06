use nom::{
    bytes::complete::take,
    combinator::map,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest,
    TimeExceeded,
    ParameterProblem,
    Timestamp,
    TimestampReply,
    InformationRequest,
    InformationReply,
    Unknown(u8),
}

#[derive(Debug)]
pub enum IcmpCode {
    Zero,
    NetUnreachable,
    HostUnreachable,
    ProtocolUnreachable,
    PortUnreachable,
    FragmentationNeeded,
    SourceRouteFailed,
    RedirectNetwork,
    RedirectHost,
    RedirectTosNetwork,
    RedirectTosHost,
    TtlExceeded,
    FragmentReassemblyTimeExceeded,
    Unknown(u8),
}

#[derive(Debug)]
pub enum RestOfHeader {
    EchoData { identifier: u16, sequence: u16 },
    ParameterProblem { pointer: u8, unused: [u8; 3] },
    RedirectGateway { gateway_addr: u32 },
    TimestampData { identifier: u16, sequence: u16 },
    None,
}

#[derive(Debug)]
pub struct IcmpPacket {
    typ: IcmpType,
    code: IcmpCode,
    checksum: u16,
    rest_of_header: RestOfHeader,
    data: Vec<u8>,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], IcmpType> {
    map(be_u8, |b| match b {
        0 => IcmpType::EchoReply,
        3 => IcmpType::DestinationUnreachable,
        4 => IcmpType::SourceQuench,
        5 => IcmpType::Redirect,
        8 => IcmpType::EchoRequest,
        11 => IcmpType::TimeExceeded,
        12 => IcmpType::ParameterProblem,
        13 => IcmpType::Timestamp,
        14 => IcmpType::TimestampReply,
        15 => IcmpType::InformationRequest,
        16 => IcmpType::InformationReply,
        n => IcmpType::Unknown(n),
    })(input)
}

fn parse_icmp_code<'a>(typ: &IcmpType, input: &'a [u8]) -> IResult<&'a [u8], IcmpCode> {
    map(be_u8, |code| match (typ, code) {
        (IcmpType::DestinationUnreachable, 0) => IcmpCode::NetUnreachable,
        (IcmpType::DestinationUnreachable, 1) => IcmpCode::HostUnreachable,
        (IcmpType::DestinationUnreachable, 2) => IcmpCode::ProtocolUnreachable,
        (IcmpType::DestinationUnreachable, 3) => IcmpCode::PortUnreachable,
        (IcmpType::DestinationUnreachable, 4) => IcmpCode::FragmentationNeeded,
        (IcmpType::DestinationUnreachable, 5) => IcmpCode::SourceRouteFailed,
        (IcmpType::Redirect, 0) => IcmpCode::RedirectNetwork,
        (IcmpType::Redirect, 1) => IcmpCode::RedirectHost,
        (IcmpType::Redirect, 2) => IcmpCode::RedirectTosNetwork,
        (IcmpType::Redirect, 3) => IcmpCode::RedirectTosHost,
        (IcmpType::TimeExceeded, 0) => IcmpCode::TtlExceeded,
        (IcmpType::TimeExceeded, 1) => IcmpCode::FragmentReassemblyTimeExceeded,
        (_, 0) => IcmpCode::Zero,
        (_, n) => IcmpCode::Unknown(n),
    })(input)
}

fn parse_rest_of_header<'a>(typ: &IcmpType, input: &'a [u8]) -> IResult<&'a [u8], RestOfHeader> {
    match typ {
        IcmpType::EchoReply | IcmpType::EchoRequest => {
            let (input, identifier) = be_u16(input)?;
            let (input, sequence) = be_u16(input)?;
            Ok((input, RestOfHeader::EchoData { identifier, sequence }))
        }
        IcmpType::ParameterProblem => {
            let (input, pointer) = be_u8(input)?;
            let (input, unused) = map(take(3usize), |b: &[u8]| [b[0], b[1], b[2]])(input)?;
            Ok((input, RestOfHeader::ParameterProblem { pointer, unused }))
        }
        IcmpType::Redirect => {
            let (input, gateway_addr) = be_u32(input)?;
            Ok((input, RestOfHeader::RedirectGateway { gateway_addr }))
        }
        IcmpType::Timestamp | IcmpType::TimestampReply => {
            let (input, identifier) = be_u16(input)?;
            let (input, sequence) = be_u16(input)?;
            Ok((input, RestOfHeader::TimestampData { identifier, sequence }))
        }
        _ => Ok((input, RestOfHeader::None)),
    }
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, typ) = parse_icmp_type(input)?;
    let (input, code) = parse_icmp_code(&typ, input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = parse_rest_of_header(&typ, input)?;
    let (input, data) = map(take(input.len()), |b: &[u8]| b.to_vec())(input)?;

    Ok((
        input,
        IcmpPacket {
            typ,
            code,
            checksum,
            rest_of_header,
            data,
        },
    ))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <icmp_binary_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_icmp_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed ICMP packet: {:?}", packet);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => eprintln!("Failed to parse ICMP packet: {:?}", e),
    }

    Ok(())
}