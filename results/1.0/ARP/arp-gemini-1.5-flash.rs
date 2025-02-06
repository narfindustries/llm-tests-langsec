use nom::{
    bytes::complete::{take, tag},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
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
    sender_hw_addr: Vec<u8>,
    sender_protocol_addr: Vec<u8>,
    target_hw_addr: Vec<u8>,
    target_protocol_addr: Vec<u8>,
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, (hardware_type, protocol_type, hardware_addr_len, protocol_addr_len, opcode)) =
        tuple((be_u16, be_u16, be_u8, be_u8, be_u16))(input)?;

    let (input, sender_hw_addr) = take(hardware_addr_len as usize)(input)?;
    let (input, sender_protocol_addr) = take(protocol_addr_len as usize)(input)?;
    let (input, target_hw_addr) = take(hardware_addr_len as usize)(input)?;
    let (input, target_protocol_addr) = take(protocol_addr_len as usize)(input)?;

    Ok((
        input,
        ArpPacket {
            hardware_type,
            protocol_type,
            hardware_addr_len,
            protocol_addr_len,
            opcode,
            sender_hw_addr: sender_hw_addr.to_vec(),
            sender_protocol_addr: sender_protocol_addr.to_vec(),
            target_hw_addr: target_hw_addr.to_vec(),
            target_protocol_addr: target_protocol_addr.to_vec(),
        },
    ))
}


fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let buffer = read(filename)?;

    match parse_arp_packet(&buffer) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => {
            eprintln!("Error parsing ARP packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}
