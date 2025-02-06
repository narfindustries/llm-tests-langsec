use nom::{
    bytes::complete::{take, tag},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::read;

#[derive(Debug)]
struct ArpPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_len: u8,
    protocol_len: u8,
    opcode: u16,
    sender_mac: Vec<u8>,
    sender_ip: Vec<u8>,
    target_mac: Vec<u8>,
    target_ip: Vec<u8>,
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, hardware_type) = be_u16(input)?;
    let (input, protocol_type) = be_u16(input)?;
    let (input, hardware_len) = be_u8(input)?;
    let (input, protocol_len) = be_u8(input)?;
    let (input, opcode) = be_u16(input)?;
    let (input, sender_mac) = take(hardware_len as usize)(input)?;
    let (input, sender_ip) = take(protocol_len as usize)(input)?;
    let (input, target_mac) = take(hardware_len as usize)(input)?;
    let (input, target_ip) = take(protocol_len as usize)(input)?;

    Ok((
        input,
        ArpPacket {
            hardware_type,
            protocol_type,
            hardware_len,
            protocol_len,
            opcode,
            sender_mac: sender_mac.to_vec(),
            sender_ip: sender_ip.to_vec(),
            target_mac: target_mac.to_vec(),
            target_ip: target_ip.to_vec(),
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
    let data = match read(filename) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file {}: {}", filename, e);
            return;
        }
    };

    match parse_arp_packet(&data) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Error parsing ARP packet: {:?}", e),
    }
}
