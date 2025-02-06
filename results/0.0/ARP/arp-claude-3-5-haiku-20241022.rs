use nom::{
    bytes::complete::{take},
    combinator::map,
    number::complete::{be_u16},
    sequence::tuple,
    IResult,
    error::ErrorKind,
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

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, (hardware_type, protocol_type, hardware_addr_len, protocol_addr_len, operation)) = 
        tuple((be_u16, be_u16, take(1usize), take(1usize), be_u16))(input)?;

    let hardware_addr_len = hardware_addr_len[0];
    let protocol_addr_len = protocol_addr_len[0];

    let (input, sender_hardware_addr) = take(hardware_addr_len as usize)(input)?;
    let (input, sender_protocol_addr) = take(protocol_addr_len as usize)(input)?;
    let (input, target_hardware_addr) = take(hardware_addr_len as usize)(input)?;
    let (input, target_protocol_addr) = take(protocol_addr_len as usize)(input)?;

    Ok((input, ArpPacket {
        hardware_type,
        protocol_type,
        hardware_addr_len,
        protocol_addr_len,
        operation,
        sender_hardware_addr: sender_hardware_addr.to_vec(),
        sender_protocol_addr: sender_protocol_addr.to_vec(),
        target_hardware_addr: target_hardware_addr.to_vec(),
        target_protocol_addr: target_protocol_addr.to_vec(),
    }))
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

    match parse_arp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("ARP Packet: {:?}", packet);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}