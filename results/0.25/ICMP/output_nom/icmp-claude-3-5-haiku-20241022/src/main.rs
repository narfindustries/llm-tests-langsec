use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    bytes::streaming::{tag, take},
    combinator::{map, opt},
    error::ErrorKind,
    multi::count,
    number::streaming::{be_u8, be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ICMPPacket {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: Option<ICMPRestHeader>,
}

#[derive(Debug)]
enum ICMPRestHeader {
    EchoRequest {
        identifier: u16,
        sequence_number: u16,
    },
    EchoReply {
        identifier: u16,
        sequence_number: u16,
    },
    DestinationUnreachable {
        unused: u32,
    },
    TimeExceeded {
        unused: u32,
    },
    Redirect {
        gateway_address: u32,
    },
    RouterAdvertisement {
        num_addresses: u8,
        entry_size: u8,
        lifetime: u16,
        addresses: Vec<u32>,
    },
    RouterSolicitation,
    Timestamp {
        identifier: u16,
        sequence_number: u16,
        originate_timestamp: u32,
        receive_timestamp: u32,
        transmit_timestamp: u32,
    },
    Other {
        data: Vec<u8>,
    },
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], ICMPPacket> {
    let (input, (icmp_type, code, checksum)) = tuple((be_u8, be_u8, be_u16))(input)?;

    let (input, rest_of_header) = match (icmp_type, code) {
        (8, 0) => map(tuple((be_u16, be_u16)), |(identifier, sequence_number)| {
            Some(ICMPRestHeader::EchoRequest {
                identifier,
                sequence_number,
            })
        })(input)?,
        (0, 0) => map(tuple((be_u16, be_u16)), |(identifier, sequence_number)| {
            Some(ICMPRestHeader::EchoReply {
                identifier,
                sequence_number,
            })
        })(input)?,
        (3, _) => map(be_u32, |unused| Some(ICMPRestHeader::DestinationUnreachable { unused }))(input)?,
        (11, _) => map(be_u32, |unused| Some(ICMPRestHeader::TimeExceeded { unused }))(input)?,
        (5, _) => map(be_u32, |gateway_address| Some(ICMPRestHeader::Redirect { gateway_address }))(input)?,
        (9, 0) => {
            let (input, (num_addresses, entry_size, lifetime)) = tuple((be_u8, be_u8, be_u16))(input)?;
            let (input, addresses) = count(be_u32, num_addresses as usize)(input)?;
            Ok((
                input,
                Some(ICMPRestHeader::RouterAdvertisement {
                    num_addresses,
                    entry_size,
                    lifetime,
                    addresses,
                }),
            ))
        }
        (10, 0) => Ok((input, Some(ICMPRestHeader::RouterSolicitation))),
        (13, 0) => map(
            tuple((be_u16, be_u16, be_u32, be_u32, be_u32)),
            |(identifier, sequence_number, originate_timestamp, receive_timestamp, transmit_timestamp)| {
                Some(ICMPRestHeader::Timestamp {
                    identifier,
                    sequence_number,
                    originate_timestamp,
                    receive_timestamp,
                    transmit_timestamp,
                })
            },
        )(input)?,
        _ => map(take(input.len()), |data: &[u8]| Some(ICMPRestHeader::Other { data: data.to_vec() }))(input)?,
    };

    Ok((
        input,
        ICMPPacket {
            icmp_type,
            code,
            checksum,
            rest_of_header,
        },
    ))
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
        Ok((_, packet)) => println!("Parsed ICMP Packet: {:?}", packet),
        Err(e) => eprintln!("Parsing error: {:?}", e),
    }

    Ok(())
}