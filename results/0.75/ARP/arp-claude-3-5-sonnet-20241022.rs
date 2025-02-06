use nom::number::complete::{be_u8, be_u16};
use nom::{IResult, combinator::map, sequence::tuple};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub struct ArpPacket {
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

fn parse_variable_length(input: &[u8], length: u8) -> IResult<&[u8], Vec<u8>> {
    let (remaining, bytes) = nom::bytes::complete::take(length as usize)(input)?;
    Ok((remaining, bytes.to_vec()))
}

fn parse_arp(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, (hardware_type, protocol_type, hardware_addr_len, protocol_addr_len, operation)) = 
        tuple((be_u16, be_u16, be_u8, be_u8, be_u16))(input)?;

    let (input, sender_hardware_addr) = parse_variable_length(input, hardware_addr_len)?;
    let (input, sender_protocol_addr) = parse_variable_length(input, protocol_addr_len)?;
    let (input, target_hardware_addr) = parse_variable_length(input, hardware_addr_len)?;
    let (input, target_protocol_addr) = parse_variable_length(input, protocol_addr_len)?;

    Ok((input, ArpPacket {
        hardware_type,
        protocol_type,
        hardware_addr_len,
        protocol_addr_len,
        operation,
        sender_hardware_addr,
        sender_protocol_addr,
        target_hardware_addr,
        target_protocol_addr,
    }))
}

impl ArpPacket {
    fn get_operation_name(&self) -> &str {
        match self.operation {
            1 => "REQUEST",
            2 => "REPLY",
            _ => "UNKNOWN",
        }
    }

    fn format_mac_address(&self, addr: &[u8]) -> String {
        addr.iter()
            .map(|b| format!("{:02x}", b))
            .collect::<Vec<String>>()
            .join(":")
    }

    fn format_ip_address(&self, addr: &[u8]) -> String {
        addr.iter()
            .map(|b| b.to_string())
            .collect::<Vec<String>>()
            .join(".")
    }

    fn display(&self) {
        println!("ARP Packet:");
        println!("Hardware Type: {:#04x}", self.hardware_type);
        println!("Protocol Type: {:#04x}", self.protocol_type);
        println!("Hardware Address Length: {}", self.hardware_addr_len);
        println!("Protocol Address Length: {}", self.protocol_addr_len);
        println!("Operation: {} ({})", self.operation, self.get_operation_name());
        println!("Sender Hardware Address: {}", self.format_mac_address(&self.sender_hardware_addr));
        println!("Sender Protocol Address: {}", self.format_ip_address(&self.sender_protocol_addr));
        println!("Target Hardware Address: {}", self.format_mac_address(&self.target_hardware_addr));
        println!("Target Protocol Address: {}", self.format_ip_address(&self.target_protocol_addr));
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

    match parse_arp(&buffer) {
        Ok((remaining, packet)) => {
            if !remaining.is_empty() {
                println!("Warning: {} bytes of trailing data", remaining.len());
            }
            packet.display();
        }
        Err(e) => {
            eprintln!("Failed to parse ARP packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}