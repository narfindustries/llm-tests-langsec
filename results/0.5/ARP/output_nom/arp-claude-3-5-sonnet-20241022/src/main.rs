use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
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
    sender_hardware_addr: Vec<u8>,
    sender_protocol_addr: Vec<u8>,
    target_hardware_addr: Vec<u8>,
    target_protocol_addr: Vec<u8>,
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, hardware_type) = be_u16(input)?;
    let (input, protocol_type) = be_u16(input)?;
    let (input, hardware_addr_len) = be_u8(input)?;
    let (input, protocol_addr_len) = be_u8(input)?;
    let (input, operation) = be_u16(input)?;
    
    let (input, sender_hardware_addr) = take(hardware_addr_len)(input)?;
    let (input, sender_protocol_addr) = take(protocol_addr_len)(input)?;
    let (input, target_hardware_addr) = take(hardware_addr_len)(input)?;
    let (input, target_protocol_addr) = take(protocol_addr_len)(input)?;

    Ok((
        input,
        ArpPacket {
            hardware_type,
            protocol_type,
            hardware_addr_len,
            protocol_addr_len,
            operation,
            sender_hardware_addr: sender_hardware_addr.to_vec(),
            sender_protocol_addr: sender_protocol_addr.to_vec(),
            target_hardware_addr: target_hardware_addr.to_vec(),
            target_protocol_addr: target_protocol_addr.to_vec(),
        },
    ))
}

fn format_mac_address(bytes: &[u8]) -> String {
    bytes
        .iter()
        .map(|b| format!("{:02x}", b))
        .collect::<Vec<String>>()
        .join(":")
}

fn format_ip_address(bytes: &[u8]) -> String {
    bytes
        .iter()
        .map(|b| b.to_string())
        .collect::<Vec<String>>()
        .join(".")
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
        Ok((remaining, packet)) => {
            println!("ARP Packet Parse Results:");
            println!("Hardware Type: {:#04x}", packet.hardware_type);
            println!("Protocol Type: {:#04x}", packet.protocol_type);
            println!("Hardware Address Length: {}", packet.hardware_addr_len);
            println!("Protocol Address Length: {}", packet.protocol_addr_len);
            println!("Operation: {}", match packet.operation {
                1 => "REQUEST",
                2 => "REPLY",
                _ => "UNKNOWN",
            });
            println!("Sender Hardware Address: {}", format_mac_address(&packet.sender_hardware_addr));
            println!("Sender Protocol Address: {}", format_ip_address(&packet.sender_protocol_addr));
            println!("Target Hardware Address: {}", format_mac_address(&packet.target_hardware_addr));
            println!("Target Protocol Address: {}", format_ip_address(&packet.target_protocol_addr));
            
            if !remaining.is_empty() {
                println!("Warning: {} bytes of trailing data", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Failed to parse ARP packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}