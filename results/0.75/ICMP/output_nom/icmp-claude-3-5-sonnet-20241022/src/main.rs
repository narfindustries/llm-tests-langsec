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
enum DestUnreachableCode {
    NetUnreachable,
    HostUnreachable,
    ProtocolUnreachable,
    PortUnreachable,
    FragmentationNeeded,
    SourceRouteFailed,
    Unknown(u8),
}

#[derive(Debug)]
enum RedirectCode {
    RedirectNetwork,
    RedirectHost,
    RedirectTosNetwork,
    RedirectTosHost,
    Unknown(u8),
}

#[derive(Debug)]
enum TimeExceededCode {
    TtlExceeded,
    FragmentReassemblyTimeExceeded,
    Unknown(u8),
}

#[derive(Debug)]
enum IcmpCode {
    DestinationUnreachable(DestUnreachableCode),
    Redirect(RedirectCode),
    TimeExceeded(TimeExceededCode),
    Default,
    Unknown(u8),
}

#[derive(Debug)]
enum RestOfHeader {
    EchoData { identifier: u16, sequence: u16 },
    Unused,
    GatewayAddress(u32),
    ParameterProblem { pointer: u8, unused: [u8; 3] },
}

#[derive(Debug)]
enum DataSection {
    ErrorMessage { header: Vec<u8>, original_data: Vec<u8> },
    Echo(Vec<u8>),
    Timestamp {
        originate: u32,
        receive: u32,
        transmit: u32,
    },
    Empty,
}

#[derive(Debug)]
struct IcmpPacket {
    icmp_type: IcmpType,
    code: IcmpCode,
    checksum: u16,
    rest_of_header: RestOfHeader,
    data: DataSection,
}

fn parse_icmp_type(input: &[u8]) -> IResult<&[u8], IcmpType> {
    map(be_u8, |t| match t {
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

fn parse_icmp_code<'a>(icmp_type: &IcmpType, input: &'a [u8]) -> IResult<&'a [u8], IcmpCode> {
    map(be_u8, |c| match icmp_type {
        IcmpType::DestinationUnreachable => IcmpCode::DestinationUnreachable(match c {
            0 => DestUnreachableCode::NetUnreachable,
            1 => DestUnreachableCode::HostUnreachable,
            2 => DestUnreachableCode::ProtocolUnreachable,
            3 => DestUnreachableCode::PortUnreachable,
            4 => DestUnreachableCode::FragmentationNeeded,
            5 => DestUnreachableCode::SourceRouteFailed,
            n => DestUnreachableCode::Unknown(n),
        }),
        IcmpType::Redirect => IcmpCode::Redirect(match c {
            0 => RedirectCode::RedirectNetwork,
            1 => RedirectCode::RedirectHost,
            2 => RedirectCode::RedirectTosNetwork,
            3 => RedirectCode::RedirectTosHost,
            n => RedirectCode::Unknown(n),
        }),
        IcmpType::TimeExceeded => IcmpCode::TimeExceeded(match c {
            0 => TimeExceededCode::TtlExceeded,
            1 => TimeExceededCode::FragmentReassemblyTimeExceeded,
            n => TimeExceededCode::Unknown(n),
        }),
        _ => {
            if c == 0 {
                IcmpCode::Default
            } else {
                IcmpCode::Unknown(c)
            }
        }
    })(input)
}

fn parse_rest_of_header<'a>(icmp_type: &IcmpType, input: &'a [u8]) -> IResult<&'a [u8], RestOfHeader> {
    match icmp_type {
        IcmpType::EchoReply | IcmpType::EchoRequest | IcmpType::Timestamp | IcmpType::TimestampReply
        | IcmpType::InformationRequest | IcmpType::InformationReply => {
            let (input, identifier) = be_u16(input)?;
            let (input, sequence) = be_u16(input)?;
            Ok((
                input,
                RestOfHeader::EchoData {
                    identifier,
                    sequence,
                },
            ))
        }
        IcmpType::Redirect => {
            let (input, addr) = be_u32(input)?;
            Ok((input, RestOfHeader::GatewayAddress(addr)))
        }
        IcmpType::ParameterProblem => {
            let (input, pointer) = be_u8(input)?;
            let (input, unused) = take(3u8)(input)?;
            Ok((
                input,
                RestOfHeader::ParameterProblem {
                    pointer,
                    unused: [unused[0], unused[1], unused[2]],
                },
            ))
        }
        _ => Ok((input, RestOfHeader::Unused)),
    }
}

fn parse_data_section<'a>(icmp_type: &IcmpType, input: &'a [u8]) -> IResult<&'a [u8], DataSection> {
    match icmp_type {
        IcmpType::DestinationUnreachable
        | IcmpType::SourceQuench
        | IcmpType::TimeExceeded
        | IcmpType::ParameterProblem => {
            let (input, header) = take(20u8)(input)?;
            let (input, original_data) = take(8u8)(input)?;
            Ok((
                input,
                DataSection::ErrorMessage {
                    header: header.to_vec(),
                    original_data: original_data.to_vec(),
                },
            ))
        }
        IcmpType::Timestamp | IcmpType::TimestampReply => {
            let (input, originate) = be_u32(input)?;
            let (input, receive) = be_u32(input)?;
            let (input, transmit) = be_u32(input)?;
            Ok((
                input,
                DataSection::Timestamp {
                    originate,
                    receive,
                    transmit,
                },
            ))
        }
        IcmpType::EchoRequest | IcmpType::EchoReply => {
            Ok((input, DataSection::Echo(input.to_vec())))
        }
        _ => Ok((input, DataSection::Empty)),
    }
}

fn parse_icmp(input: &[u8]) -> IResult<&[u8], IcmpPacket> {
    let (input, icmp_type) = parse_icmp_type(input)?;
    let (input, code) = parse_icmp_code(&icmp_type, input)?;
    let (input, checksum) = be_u16(input)?;
    let (input, rest_of_header) = parse_rest_of_header(&icmp_type, input)?;
    let (input, data) = parse_data_section(&icmp_type, input)?;

    Ok((
        input,
        IcmpPacket {
            icmp_type,
            code,
            checksum,
            rest_of_header,
            data,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_icmp(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed ICMP packet: {:?}", packet);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => eprintln!("Failed to parse ICMP packet: {:?}", e),
    }
}