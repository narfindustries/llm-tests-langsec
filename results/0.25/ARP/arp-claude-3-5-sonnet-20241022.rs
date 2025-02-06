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

impl ArpPacket {
    fn display_hardware_type(&self) -> String {
        match self.hardware_type {
            1 => "Ethernet (10Mb)".to_string(),
            6 => "IEEE 802 Networks".to_string(),
            7 => "ARCNET".to_string(),
            _ => format!("Unknown ({})", self.hardware_type),
        }
    }

    fn display_protocol_type(&self) -> String {
        match self.protocol_type {
            0x0800 => "IPv4".to_string(),
            0x86DD => "IPv6".to_string(),
            _ => format!("Unknown (0x{:04X})", self.protocol_type),
        }
    }

    fn display_operation(&self) -> String {
        match self.operation {
            1 => "ARP Request".to_string(),
            2 => "ARP Reply".to_string(),
            _ => format!("Unknown ({})", self.operation),
        }
    }

    fn display_mac_address(addr: &[u8]) -> String {
        addr.iter()
            .map(|b| format!("{:02X}", b))
            .collect::<Vec<String>>()
            .join(":")
    }

    fn display_ip_address(addr: &[u8]) -> String {
        addr.iter()
            .map(|b| b.to_string())
            .collect::<Vec<String>>()
            .join(".")
    }
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
            println!("Hardware Type: {}", packet.display_hardware_type());
            println!("Protocol Type: {}", packet.display_protocol_type());
            println!("Hardware Address Length: {} bytes", packet.hardware_addr_len);
            println!("Protocol Address Length: {} bytes", packet.protocol_addr_len);
            println!("Operation: {}", packet.display_operation());
            println!(
                "Sender Hardware Address: {}",
                ArpPacket::display_mac_address(&packet.sender_hardware_addr)
            );
            println!(
                "Sender Protocol Address: {}",
                ArpPacket::display_ip_address(&packet.sender_protocol_addr)
            );
            println!(
                "Target Hardware Address: {}",
                ArpPacket::display_mac_address(&packet.target_hardware_addr)
            );
            println!(
                "Target Protocol Address: {}",
                ArpPacket::display_ip_address(&packet.target_protocol_addr)
            );
            
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