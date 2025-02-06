use nom::{
    bytes::complete::{take, tag},
    combinator::{map, map_res},
    number::complete::be_u16,
    IResult,
};
use std::env;
use std::fs::read;

#[derive(Debug)]
struct ArpPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_addr_len: u8,
    protocol_addr_len: u8,
    opcode: u16,
    sender_hardware_addr: Vec<u8>,
    sender_protocol_addr: Vec<u8>,
    target_hardware_addr: Vec<u8>,
    target_protocol_addr: Vec<u8>,
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, hardware_type) = be_u16(input)?;
    let (input, protocol_type) = be_u16(input)?;
    let (input, hardware_addr_len) = map(take(1usize), |x: &[u8]| x[0])(input)?;
    let (input, protocol_addr_len) = map(take(1usize), |x: &[u8]| x[0])(input)?;
    let (input, opcode) = be_u16(input)?;
    let (input, sender_hardware_addr) = take(hardware_addr_len as usize)(input)?;
    let (input, sender_protocol_addr) = take(protocol_addr_len as usize)(input)?;
    let (input, target_hardware_addr) = take(hardware_addr_len as usize)(input)?;
    let (input, target_protocol_addr) = take(protocol_addr_len as usize)(input)?;

    Ok((
        input,
        ArpPacket {
            hardware_type,
            protocol_type,
            hardware_addr_len,
            protocol_addr_len,
            opcode,
            sender_hardware_addr: sender_hardware_addr.to_vec(),
            sender_protocol_addr: sender_protocol_addr.to_vec(),
            target_hardware_addr: target_hardware_addr.to_vec(),
            target_protocol_addr: target_protocol_addr.to_vec(),
        },
    ))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    match read(filename) {
        Ok(data) => {
            match parse_arp_packet(&data) {
                Ok((_, packet)) => println!("{:?}", packet),
                Err(e) => eprintln!("Error parsing ARP packet: {:?}", e),
            }
        }
        Err(e) => eprintln!("Error reading file: {}", e),
    }
}
