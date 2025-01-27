use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ArpPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_len: u8,
    protocol_len: u8,
    opcode: u16,
    sender_mac: [u8; 6],
    sender_ip: u32,
    target_mac: [u8; 6],
    target_ip: u32,
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, hardware_type) = be_u16(input)?;
    let (input, protocol_type) = be_u16(input)?;
    let (input, hardware_len) = take(1usize)(input)?;
    let (input, protocol_len) = take(1usize)(input)?;
    let (input, opcode) = be_u16(input)?;
    let (input, sender_mac) = take(6usize)(input)?;
    let (input, sender_ip) = be_u32(input)?;
    let (input, target_mac) = take(6usize)(input)?;
    let (input, target_ip) = be_u32(input)?;

    Ok((
        input,
        ArpPacket {
            hardware_type: hardware_type,
            protocol_type: protocol_type,
            hardware_len: hardware_len[0],
            protocol_len: protocol_len[0],
            opcode: opcode,
            sender_mac: sender_mac.try_into().unwrap(),
            sender_ip: sender_ip,
            target_mac: target_mac.try_into().unwrap(),
            target_ip: target_ip,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_arp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => println!("Error parsing ARP packet: {:?}", e),
    }
}
