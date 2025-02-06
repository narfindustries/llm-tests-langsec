use nom::number::complete::{be_u8, be_u16, be_u32};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum IcmpType {
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
enum IcmpCode {
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
enum RestOfHeader {
    EchoData { identifier: u16, sequence_number: u16 },
    Unused(u32),
    GatewayInternetAddress(u32),
    ParameterProblem { pointer: u8, unused: [u8; 3] },
}

#[derive(Debug)]
struct IcmpPacket {
    icmp_type: IcmpType,
    code: IcmpCode,
    checksum: u16,
    rest_of_header: RestOfHeader,
    data: Vec<u8>,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], IcmpType> {
    let (input, type_val) = be_u8(input)?;
    let icmp_type = match type_val {
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
    };
    Ok((input, icmp_type))
}

fn parse_icmp_code<'a>(input: &'a [u8], icmp_type: &IcmpType) -> IResult<&'a [u8], IcmpCode> {
    let (input, code_val) = be_u8(input)?;
    let code = match (icmp_type, code_val) {
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
    };
    Ok((input, code))
}

fn parse_rest_of_header<'a>(input: &'a [u8], icmp_type: &IcmpType) -> IResult<&'a [u8], RestOfHeader> {
    match icmp_type {
        IcmpType::EchoReply | IcmpType::EchoRequest => {
            let (input, identifier) = be_u16(input)?;
            let (input, sequence_number) = be_u16(input)?;
            Ok((
                input,
                RestOfHeader::EchoData {
                    identifier,
                    sequence_number,
                },
            ))
        }
        IcmpType::Redirect => {
            let (input, gateway_addr) = be_u32(input)?;
            Ok((input, RestOfHeader::GatewayInternetAddress(gateway_addr)))
        }
        IcmpType::ParameterProblem => {
            let (input, pointer) = be_u8(input)?;
            let (input, b1) = be_u8(input)?;
            let (input, b2) = be_u8(input)?;
            let (input, b3) = be_u8(input)?;
            Ok((
                input,
                RestOfHeader::ParameterProblem {
                    pointer,
                    unused: [b1, b2, b3],
                },
            ))
        }
        _ => {
            let (input, unused) = be_u32(input)?;
            Ok((input, RestOfHeader::Unused(unused)))
        }
    }
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = parse_icmp_code(input, &icmp_type)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = parse_rest_of_header(input, &icmp_type)?;
    Ok((
        &input[..0],
        IcmpPacket {
            icmp_type,
            code,
            checksum,
            rest_of_header,
            data: input.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <icmp_binary_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)
        .expect("Failed to read file contents");

    match parse_icmp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse ICMP packet: {:?}", e),
    }
}