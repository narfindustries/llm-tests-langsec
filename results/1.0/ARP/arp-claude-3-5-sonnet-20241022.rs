extern crate nom;
use nom::{
    bits::complete::{tag, take},
    combinator::map,
    error::Error,
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
    hardware_size: u8,
    protocol_size: u8,
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
        hardware_size,
        protocol_size,
        operation,
    )) = tuple((
        nom::number::complete::be_u16,
        nom::number::complete::be_u16,
        nom::number::complete::u8,
        nom::number::complete::u8,
        nom::number::complete::be_u16,
    ))(input)?;

    let (input, sender_hardware_addr) = nom::bytes::complete::take(hardware_size as usize)(input)?;
    let (input, sender_protocol_addr) = nom::bytes::complete::take(protocol_size as usize)(input)?;
    let (input, target_hardware_addr) = nom::bytes::complete::take(hardware_size as usize)(input)?;
    let (input, target_protocol_addr) = nom::bytes::complete::take(protocol_size as usize)(input)?;

    Ok((
        input,
        ArpPacket {
            hardware_type,
            protocol_type,
            hardware_size,
            protocol_size,
            operation,
            sender_hardware_addr: sender_hardware_addr.to_vec(),
            sender_protocol_addr: sender_protocol_addr.to_vec(),
            target_hardware_addr: target_hardware_addr.to_vec(),
            target_protocol_addr: target_protocol_addr.to_vec(),
        },
    ))
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
            println!("Remaining bytes: {:?}", remaining);
        }
        Err(e) => {
            eprintln!("Failed to parse ARP packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}