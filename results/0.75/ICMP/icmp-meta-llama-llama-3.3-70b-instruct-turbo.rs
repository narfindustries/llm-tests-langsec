use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, map_res, opt},
    error::context,
    multi::take_while_m_n,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{
    fs::File,
    io::{self, Read},
    path::Path,
};

#[derive(Debug)]
enum IcmpType {
    EchoReply,
    DestinationUnreachable,
    SourceQuench,
    Redirect,
    EchoRequest,
    TimeExceeded,
    ParameterProblem,
    TimestampRequest,
    TimestampReply,
    InfoRequest,
    InfoReply,
    AddressRequest,
    AddressReply,
}

impl IcmpType {
    fn from_u8(n: u8) -> Option<Self> {
        match n {
            0 => Some(IcmpType::EchoReply),
            3 => Some(IcmpType::DestinationUnreachable),
            4 => Some(IcmpType::SourceQuench),
            5 => Some(IcmpType::Redirect),
            8 => Some(IcmpType::EchoRequest),
            11 => Some(IcmpType::TimeExceeded),
            12 => Some(IcmpType::ParameterProblem),
            13 => Some(IcmpType::TimestampRequest),
            14 => Some(IcmpType::TimestampReply),
            15 => Some(IcmpType::InfoRequest),
            16 => Some(IcmpType::InfoReply),
            17 => Some(IcmpType::AddressRequest),
            18 => Some(IcmpType::AddressReply),
            _ => None,
        }
    }
}

#[derive(Debug)]
struct IcmpHeader {
    type_: IcmpType,
    code: u8,
    checksum: u16,
    identifier: u16,
    sequence_number: u16,
}

impl IcmpHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        context("icmp header", move |input| {
            let (input, type_) = map_res(be_u8, IcmpType::from_u8)(input)?;
            let (input, code) = be_u8(input)?;
            let (input, checksum) = be_u16(input)?;
            let (input, identifier) = be_u16(input)?;
            let (input, sequence_number) = be_u16(input)?;
            Ok((input, IcmpHeader {
                type_,
                code,
                checksum,
                identifier,
                sequence_number,
            }))
        })(input)
    }
}

#[derive(Debug)]
enum IcmpMessage {
    EchoRequest {
        header: IcmpHeader,
        timestamp: u32,
    },
    EchoReply {
        header: IcmpHeader,
        timestamp: u32,
    },
    DestinationUnreachable {
        header: IcmpHeader,
        unused: u32,
        ip_header: [u8; 28],
    },
    Redirect {
        header: IcmpHeader,
        gateway_address: u32,
        ip_header: [u8; 28],
    },
    TimeExceeded {
        header: IcmpHeader,
        unused: u32,
        ip_header: [u8; 28],
    },
    ParameterProblem {
        header: IcmpHeader,
        pointer: u8,
        unused: u32,
        ip_header: [u8; 28],
    },
    TimestampRequest {
        header: IcmpHeader,
        originate_timestamp: u32,
        receive_timestamp: u32,
        transmit_timestamp: u32,
    },
    TimestampReply {
        header: IcmpHeader,
        originate_timestamp: u32,
        receive_timestamp: u32,
        transmit_timestamp: u32,
    },
}

impl IcmpMessage {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        context("icmp message", move |input| {
            let (input, header) = IcmpHeader::parse(input)?;
            match header.type_ {
                IcmpType::EchoRequest => {
                    let (input, timestamp) = be_u32(input)?;
                    Ok((input, IcmpMessage::EchoRequest { header, timestamp }))
                }
                IcmpType::EchoReply => {
                    let (input, timestamp) = be_u32(input)?;
                    Ok((input, IcmpMessage::EchoReply { header, timestamp }))
                }
                IcmpType::DestinationUnreachable => {
                    let (input, unused) = be_u32(input)?;
                    let (input, ip_header) = take(28u8)(input)?;
                    Ok((
                        input,
                        IcmpMessage::DestinationUnreachable {
                            header,
                            unused,
                            ip_header: ip_header.try_into().unwrap(),
                        },
                    ))
                }
                IcmpType::Redirect => {
                    let (input, gateway_address) = be_u32(input)?;
                    let (input, ip_header) = take(28u8)(input)?;
                    Ok((
                        input,
                        IcmpMessage::Redirect {
                            header,
                            gateway_address,
                            ip_header: ip_header.try_into().unwrap(),
                        },
                    ))
                }
                IcmpType::TimeExceeded => {
                    let (input, unused) = be_u32(input)?;
                    let (input, ip_header) = take(28u8)(input)?;
                    Ok((
                        input,
                        IcmpMessage::TimeExceeded {
                            header,
                            unused,
                            ip_header: ip_header.try_into().unwrap(),
                        },
                    ))
                }
                IcmpType::ParameterProblem => {
                    let (input, pointer) = be_u8(input)?;
                    let (input, unused) = be_u32(input)?;
                    let (input, ip_header) = take(28u8)(input)?;
                    Ok((
                        input,
                        IcmpMessage::ParameterProblem {
                            header,
                            pointer,
                            unused,
                            ip_header: ip_header.try_into().unwrap(),
                        },
                    ))
                }
                IcmpType::TimestampRequest => {
                    let (input, originate_timestamp) = be_u32(input)?;
                    let (input, receive_timestamp) = be_u32(input)?;
                    let (input, transmit_timestamp) = be_u32(input)?;
                    Ok((
                        input,
                        IcmpMessage::TimestampRequest {
                            header,
                            originate_timestamp,
                            receive_timestamp,
                            transmit_timestamp,
                        },
                    ))
                }
                IcmpType::TimestampReply => {
                    let (input, originate_timestamp) = be_u32(input)?;
                    let (input, receive_timestamp) = be_u32(input)?;
                    let (input, transmit_timestamp) = be_u32(input)?;
                    Ok((
                        input,
                        IcmpMessage::TimestampReply {
                            header,
                            originate_timestamp,
                            receive_timestamp,
                            transmit_timestamp,
                        },
                    ))
                }
                _ => Err(nom::Err::Error(nom::error::Error::Missing)),
            }
        })(input)
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut input = Vec::new();
    file.read_to_end(&mut input)?;
    match IcmpMessage::parse(&input) {
        Ok((_, msg)) => println!("{:?}", msg),
        Err(e) => println!("Error: {:?}", e),
    }
    Ok(())
}