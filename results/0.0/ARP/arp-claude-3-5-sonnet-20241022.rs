use nom::{
    bytes::complete::take,
    combinator::map,
    number::complete::{be_u16, be_u8},
    IResult,
};
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

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, hardware_type) = be_u16(input)?;
    let (input, protocol_type) = be_u16(input)?;
    let (input, hardware_addr_len) = be_u8(input)?;
    let (input, protocol_addr_len) = be_u8(input)?;
    let (input, operation) = be_u16(input)?;
    
    let (input, sender_hardware_addr) = map(
        take(hardware_addr_len),
        |bytes: &[u8]| bytes.to_vec()
    )(input)?;
    
    let (input, sender_protocol_addr) = map(
        take(protocol_addr_len),
        |bytes: &[u8]| bytes.to_vec()
    )(input)?;
    
    let (input, target_hardware_addr) = map(
        take(hardware_addr_len),
        |bytes: &[u8]| bytes.to_vec()
    )(input)?;
    
    let (input, target_protocol_addr) = map(
        take(protocol_addr_len),
        |bytes: &[u8]| bytes.to_vec()
    )(input)?;

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
    fn display_addresses(&self) {
        println!("Hardware Type: {:#04x}", self.hardware_type);
        println!("Protocol Type: {:#04x}", self.protocol_type);
        println!("Hardware Address Length: {}", self.hardware_addr_len);
        println!("Protocol Address Length: {}", self.protocol_addr_len);
        println!("Operation: {}", self.operation);
        
        print!("Sender Hardware Address: ");
        for byte in &self.sender_hardware_addr {
            print!("{:02x}:", byte);
        }
        println!();
        
        print!("Sender Protocol Address: ");
        for byte in &self.sender_protocol_addr {
            print!("{}.", byte);
        }
        println!();
        
        print!("Target Hardware Address: ");
        for byte in &self.target_hardware_addr {
            print!("{:02x}:", byte);
        }
        println!();
        
        print!("Target Protocol Address: ");
        for byte in &self.target_protocol_addr {
            print!("{}.", byte);
        }
        println!();
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
            if !remaining.is_empty() {
                println!("Warning: {} bytes of trailing data", remaining.len());
            }
            packet.display_addresses();
        }
        Err(e) => {
            eprintln!("Failed to parse ARP packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}