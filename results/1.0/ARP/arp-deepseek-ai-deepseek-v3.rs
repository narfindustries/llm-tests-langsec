use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct ARPPacket {
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

fn parse_hardware_type(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn parse_protocol_type(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn parse_hardware_addr_len(input: &[u8]) -> IResult<&[u8], u8> {
    be_u8(input)
}

fn parse_protocol_addr_len(input: &[u8]) -> IResult<&[u8], u8> {
    be_u8(input)
}

fn parse_operation(input: &[u8]) -> IResult<&[u8], u16> {
    be_u16(input)
}

fn parse_mac_address(input: &[u8], len: u8) -> IResult<&[u8], Vec<u8>> {
    take(len)(input).map(|(i, o)| (i, o.to_vec()))
}

fn parse_ipv4_address(input: &[u8], len: u8) -> IResult<&[u8], Vec<u8>> {
    take(len)(input).map(|(i, o)| (i, o.to_vec()))
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ARPPacket> {
    let (input, hardware_type) = parse_hardware_type(input)?;
    let (input, protocol_type) = parse_protocol_type(input)?;
    let (input, hardware_addr_len) = parse_hardware_addr_len(input)?;
    let (input, protocol_addr_len) = parse_protocol_addr_len(input)?;
    let (input, operation) = parse_operation(input)?;
    let (input, sender_hardware_addr) = parse_mac_address(input, hardware_addr_len)?;
    let (input, sender_protocol_addr) = parse_ipv4_address(input, protocol_addr_len)?;
    let (input, target_hardware_addr) = parse_mac_address(input, hardware_addr_len)?;
    let (input, target_protocol_addr) = parse_ipv4_address(input, protocol_addr_len)?;

    Ok((
        input,
        ARPPacket {
            hardware_type,
            protocol_type,
            hardware_addr_len,
            protocol_addr_len,
            operation,
            sender_hardware_addr,
            sender_protocol_addr,
            target_hardware_addr,
            target_protocol_addr,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_arp_packet(&buffer) {
        Ok((_, arp_packet)) => println!("{:?}", arp_packet),
        Err(e) => eprintln!("Failed to parse ARP packet: {:?}", e),
    }
}