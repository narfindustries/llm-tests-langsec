use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
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
    Default,
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
    FragmentReassemblyExceeded,
    Unknown(u8),
}

#[derive(Debug)]
pub enum RestOfHeader {
    EchoData { identifier: u16, sequence: u16 },
    Unused(u32),
    GatewayAddress(u32),
    ParameterProblem { pointer: u8, unused: [u8; 3] },
}

#[derive(Debug)]
pub enum IcmpData {
    ErrorMessage { ip_header: Vec<u8>, original_data: Vec<u8> },
    EchoData(Vec<u8>),
    TimestampData { orig_timestamp: u32, recv_timestamp: u32, trans_timestamp: u32 },
    Empty,
}

#[derive(Debug)]
pub struct IcmpPacket {
    typ: IcmpType,
    code: IcmpCode,
    checksum: u16,
    rest_of_header: RestOfHeader,
    data: IcmpData,
}

fn parse_type(input: u8) -> IcmpType {
    match input {
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
    }
}

fn parse_code(typ: &IcmpType, code: u8) -> IcmpCode {
    match (typ, code) {
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
        (IcmpType::TimeExceeded, 1) => IcmpCode::FragmentReassemblyExceeded,
        (_, 0) => IcmpCode::Default,
        (_, n) => IcmpCode::Unknown(n),
    }
}

fn parse_rest_of_header<'a>(typ: &IcmpType, input: &'a [u8]) -> IResult<&'a [u8], RestOfHeader> {
    match typ {
        IcmpType::EchoReply | IcmpType::EchoRequest | IcmpType::Timestamp |
        IcmpType::TimestampReply | IcmpType::InformationRequest | IcmpType::InformationReply => {
            let (input, (identifier, sequence)) = tuple((be_u16, be_u16))(input)?;
            Ok((input, RestOfHeader::EchoData { identifier, sequence }))
        },
        IcmpType::DestinationUnreachable | IcmpType::SourceQuench | IcmpType::TimeExceeded => {
            let (input, unused) = be_u32(input)?;
            Ok((input, RestOfHeader::Unused(unused)))
        },
        IcmpType::Redirect => {
            let (input, gateway) = be_u32(input)?;
            Ok((input, RestOfHeader::GatewayAddress(gateway)))
        },
        IcmpType::ParameterProblem => {
            let (input, pointer) = be_u8(input)?;
            let (input, unused) = take(3usize)(input)?;
            Ok((input, RestOfHeader::ParameterProblem {
                pointer,
                unused: [unused[0], unused[1], unused[2]],
            }))
        },
        _ => {
            let (input, unused) = be_u32(input)?;
            Ok((input, RestOfHeader::Unused(unused)))
        },
    }
}

fn parse_data<'a>(typ: &IcmpType, input: &'a [u8]) -> IResult<&'a [u8], IcmpData> {
    match typ {
        IcmpType::DestinationUnreachable | IcmpType::TimeExceeded | IcmpType::ParameterProblem => {
            let (input, ip_header) = take(20usize)(input)?;
            let (input, original_data) = take(8usize)(input)?;
            Ok((input, IcmpData::ErrorMessage {
                ip_header: ip_header.to_vec(),
                original_data: original_data.to_vec(),
            }))
        },
        IcmpType::Timestamp | IcmpType::TimestampReply => {
            let (input, (orig, recv, trans)) = tuple((be_u32, be_u32, be_u32))(input)?;
            Ok((input, IcmpData::TimestampData {
                orig_timestamp: orig,
                recv_timestamp: recv,
                trans_timestamp: trans,
            }))
        },
        IcmpType::EchoRequest | IcmpType::EchoReply => {
            Ok((b"", IcmpData::EchoData(input.to_vec())))
        },
        _ => Ok((input, IcmpData::Empty)),
    }
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, type_val) = be_u8(input)?;
    let typ = parse_type(type_val);
    
    let (input, code_val) = be_u8(input)?;
    let code = parse_code(&typ, code_val);
    
    let (input, checksum) = be_u16(input)?;
    
    let (input, rest_of_header) = parse_rest_of_header(&typ, input)?;
    
    let (input, data) = parse_data(&typ, input)?;
    
    Ok((input, IcmpPacket {
        typ,
        code,
        checksum,
        rest_of_header,
        data,
    }))
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

    match parse_icmp(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed ICMP packet: {:#?}", packet);
            println!("Remaining bytes: {} bytes", remaining.len());
        },
        Err(e) => eprintln!("Failed to parse ICMP packet: {}", e),
    }

    Ok(())
}