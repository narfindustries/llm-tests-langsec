use nom::{
    IResult,
    bytes::complete::{take},
    number::complete::{be_u16, be_u8},
    sequence::{tuple},
    combinator::{map},
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
    sender_mac: [u8; 6],
    sender_ip: [u8; 4],
    target_mac: [u8; 6],
    target_ip: [u8; 4],
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    map(
        tuple((
            be_u16,        // hardware type
            be_u16,        // protocol type
            be_u8,         // hardware address length
            be_u8,         // protocol address length
            be_u16,        // operation
            take(6usize),  // sender MAC
            take(4usize),  // sender IP
            take(6usize),  // target MAC
            take(4usize)   // target IP
        )),
        |(hardware_type, protocol_type, hardware_addr_len, protocol_addr_len, operation, 
          sender_mac, sender_ip, target_mac, target_ip)| {
            ArpPacket {
                hardware_type,
                protocol_type,
                hardware_addr_len,
                protocol_addr_len,
                operation,
                sender_mac: sender_mac.try_into().unwrap(),
                sender_ip: sender_ip.try_into().unwrap(),
                target_mac: target_mac.try_into().unwrap(),
                target_ip: target_ip.try_into().unwrap(),
            }
        }
    )(input)
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