use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ArpPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_addr_len: u8,
    protocol_addr_len: u8,
    operation: u16,
    sender_hardware_addr: Vec<u8>,
    sender_protocol_addr: Vec<u8>,
    target_hardware_addr: Vec<u8>,
    target_protocol_addr: Vec<u8>,
}

fn parse_arp(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, (
        hardware_type,
        protocol_type,
        hardware_addr_len,
        protocol_addr_len,
        operation
    )) = tuple((
        be_u16,
        be_u16,
        be_u8,
        be_u8,
        be_u16
    ))(input)?;

    let (input, sender_hardware_addr) = map(
        take(hardware_addr_len),
        |bytes: &[u8]| bytes.to_vec()
    )(input)?;

    let (input, sender_protocol_addr) = map(
        take(protocol_addr_len),
        |bytes: &[u8]| bytes.to_vec()
    )(input)?;

    let (input, target_hardware_addr) = map(
        take(hardware_addr_len),
        |bytes: &[u8]| bytes.to_vec()
    )(input)?;

    let (input, target_protocol_addr) = map(
        take(protocol_addr_len),
        |bytes: &[u8]| bytes.to_vec()
    )(input)?;

    Ok((input, ArpPacket {
        hardware_type,
        protocol_type,
        hardware_addr_len,
        protocol_addr_len,
        operation,
        sender_hardware_addr,
        sender_protocol_addr,
        target_hardware_addr,
        target_protocol_addr,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <arp_packet_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_arp(&buffer) {
        Ok((remaining, packet)) => {
            println!("ARP Packet: {:?}", packet);
            if !remaining.is_empty() {
                println!("Warning: {} bytes of unparsed data", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Failed to parse ARP packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}