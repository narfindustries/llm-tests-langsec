use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
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
    sender_mac: Vec<u8>,
    sender_ip: Vec<u8>,
    target_mac: Vec<u8>,
    target_ip: Vec<u8>,
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    map(
        tuple((
            be_u16,   // hardware type
            be_u16,   // protocol type
            be_u8,    // hardware address length
            be_u8,    // protocol address length
            be_u16,   // operation
            |i| count(be_u8, input[4] as usize)(i),   // sender MAC
            |i| count(be_u8, input[5] as usize)(i),   // sender IP
            |i| count(be_u8, input[4] as usize)(i),   // target MAC
            |i| count(be_u8, input[5] as usize)(i),   // target IP
        )),
        |(hardware_type, protocol_type, hardware_addr_len, protocol_addr_len, operation, sender_mac, sender_ip, target_mac, target_ip)| ArpPacket {
            hardware_type,
            protocol_type,
            hardware_addr_len,
            protocol_addr_len,
            operation,
            sender_mac,
            sender_ip,
            target_mac,
            target_ip,
        },
    )(input)
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
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}