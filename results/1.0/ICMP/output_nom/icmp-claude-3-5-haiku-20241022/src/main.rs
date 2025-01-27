use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    bytes::streaming::{take, tag},
    combinator::{map, complete},
    error::ErrorKind,
    multi::count,
    number::streaming::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult, Parser,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ICMPPacket {
    icmp_type: u8,
    code: u8,
    checksum: u16,
    rest_of_header: Option<ICMPHeader>,
    payload: Option<Vec<u8>>,
}

#[derive(Debug)]
enum ICMPHeader {
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
    Other {
        rest_of_header: u32,
    },
}

fn parse_icmp_packet(input: &[u8]) -> IResult<&[u8], ICMPPacket> {
    let (input, (icmp_type, code, checksum)) = tuple((be_u8, be_u8, be_u16))(input)?;

    let (input, (rest_of_header, payload)) = match icmp_type {
        8 => {
            let (input, (identifier, sequence_number)) = tuple((be_u16, be_u16))(input)?;
            let (input, payload) = take(input.len())(input)?;
            (input, (
                Some(ICMPHeader::EchoRequest { identifier, sequence_number }),
                Some(payload.to_vec())
            ))
        },
        0 => {
            let (input, (identifier, sequence_number)) = tuple((be_u16, be_u16))(input)?;
            let (input, payload) = take(input.len())(input)?;
            (input, (
                Some(ICMPHeader::EchoReply { identifier, sequence_number }),
                Some(payload.to_vec())
            ))
        },
        3 => {
            let (input, unused) = be_u32(input)?;
            let (input, payload) = take(input.len())(input)?;
            (input, (
                Some(ICMPHeader::DestinationUnreachable { unused }),
                Some(payload.to_vec())
            ))
        },
        11 => {
            let (input, unused) = be_u32(input)?;
            let (input, payload) = take(input.len())(input)?;
            (input, (
                Some(ICMPHeader::TimeExceeded { unused }),
                Some(payload.to_vec())
            ))
        },
        5 => {
            let (input, gateway_address) = be_u32(input)?;
            let (input, payload) = take(input.len())(input)?;
            (input, (
                Some(ICMPHeader::Redirect { gateway_address }),
                Some(payload.to_vec())
            ))
        },
        9 => {
            let (input, (num_addresses, entry_size, lifetime)) = tuple((be_u8, be_u8, be_u16))(input)?;
            let (input, addresses) = count(be_u32, num_addresses as usize)(input)?;
            let (input, payload) = take(input.len())(input)?;
            (input, (
                Some(ICMPHeader::RouterAdvertisement { 
                    num_addresses, 
                    entry_size, 
                    lifetime, 
                    addresses 
                }),
                Some(payload.to_vec())
            ))
        },
        10 => {
            let (input, payload) = take(input.len())(input)?;
            (input, (
                Some(ICMPHeader::RouterSolicitation),
                Some(payload.to_vec())
            ))
        },
        _ => {
            let (input, rest_of_header) = be_u32(input)?;
            let (input, payload) = take(input.len())(input)?;
            (input, (
                Some(ICMPHeader::Other { rest_of_header }),
                Some(payload.to_vec())
            ))
        }
    };

    Ok((input, ICMPPacket {
        icmp_type,
        code,
        checksum,
        rest_of_header,
        payload,
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

    match parse_icmp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed ICMP Packet: {:?}", packet);
            Ok(())
        },
        Err(e) => {
            eprintln!("Failed to parse ICMP packet: {:?}", e);
            std::process::exit(1);
        }
    }
}