use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    number::complete::{be_u8, be_u16},
    bytes::complete::take,
};

#[derive(Debug)]
pub struct ArpPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_len: u8,
    protocol_len: u8,
    operation: u16,
    sender_hardware_addr: Vec<u8>,
    sender_protocol_addr: Vec<u8>,
    target_hardware_addr: Vec<u8>,
    target_protocol_addr: Vec<u8>,
}

fn parse_arp(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, hardware_type) = be_u16(input)?;
    let (input, protocol_type) = be_u16(input)?;
    let (input, hardware_len) = be_u8(input)?;
    let (input, protocol_len) = be_u8(input)?;
    let (input, operation) = be_u16(input)?;

    let (input, sender_hardware_addr) = take(hardware_len)(input)?;
    let (input, sender_protocol_addr) = take(protocol_len)(input)?;
    let (input, target_hardware_addr) = take(hardware_len)(input)?;
    let (input, target_protocol_addr) = take(protocol_len)(input)?;

    Ok((input, ArpPacket {
        hardware_type,
        protocol_type,
        hardware_len,
        protocol_len,
        operation,
        sender_hardware_addr: sender_hardware_addr.to_vec(),
        sender_protocol_addr: sender_protocol_addr.to_vec(),
        target_hardware_addr: target_hardware_addr.to_vec(),
        target_protocol_addr: target_protocol_addr.to_vec(),
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_arp(&buffer) {
        Ok((_, arp_packet)) => println!("{:?}", arp_packet),
        Err(err) => eprintln!("Failed to parse ARP packet: {:?}", err),
    }
}