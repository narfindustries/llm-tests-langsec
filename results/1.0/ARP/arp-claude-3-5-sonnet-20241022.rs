use nom::combinator::map;
use nom::number::complete::{be_u16, be_u8};
use nom::sequence::tuple;
use nom::{IResult, Parser};
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
    let (input, bytes) = nom::bytes::complete::take(length as usize)(input)?;
    Ok((input, bytes.to_vec()))
}

fn parse_arp(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, (hardware_type, protocol_type, hardware_addr_len, protocol_addr_len, operation)) =
        tuple((be_u16, be_u16, be_u8, be_u8, be_u16))(input)?;

    let parser = move |input| {
        let (input, sender_hardware_addr) = parse_variable_length(input, hardware_addr_len)?;
        let (input, sender_protocol_addr) = parse_variable_length(input, protocol_addr_len)?;
        let (input, target_hardware_addr) = parse_variable_length(input, hardware_addr_len)?;
        let (input, target_protocol_addr) = parse_variable_length(input, protocol_addr_len)?;

        Ok((
            input,
            ArpPacket {
                hardware_type,
                protocol_type,
                hardware_addr_len,
                protocol_addr_len,
                operation,
                sender_hardware_addr,
                sender_protocol_addr,
                target_hardware_addr,
                target_protocol_addr,
            },
        ))
    };

    parser(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
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
            println!("Successfully parsed ARP packet:");
            println!("Hardware Type: {:#04x}", packet.hardware_type);
            println!("Protocol Type: {:#04x}", packet.protocol_type);
            println!("Hardware Address Length: {}", packet.hardware_addr_len);
            println!("Protocol Address Length: {}", packet.protocol_addr_len);
            println!("Operation: {}", packet.operation);
            println!(
                "Sender Hardware Address: {:02x?}",
                packet.sender_hardware_addr
            );
            println!(
                "Sender Protocol Address: {:02x?}",
                packet.sender_protocol_addr
            );
            println!(
                "Target Hardware Address: {:02x?}",
                packet.target_hardware_addr
            );
            println!(
                "Target Protocol Address: {:02x?}",
                packet.target_protocol_addr
            );
            
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining unparsed", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Failed to parse ARP packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_arp() {
        // Example ARP packet (28 bytes)
        let test_data = [
            0x00, 0x01, // Hardware type: Ethernet
            0x08, 0x00, // Protocol type: IPv4
            0x06, // Hardware address length
            0x04, // Protocol address length
            0x00, 0x01, // Operation: Request
            0x11, 0x22, 0x33, 0x44, 0x55, 0x66, // Sender hardware address
            0xc0, 0xa8, 0x01, 0x01, // Sender protocol address
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, // Target hardware address
            0xc0, 0xa8, 0x01, 0x02, // Target protocol address
        ];

        let result = parse_arp(&test_data);
        assert!(result.is_ok());

        let (remaining, packet) = result.unwrap();
        assert!(remaining.is_empty());
        assert_eq!(packet.hardware_type, 1);
        assert_eq!(packet.protocol_type, 0x0800);
        assert_eq!(packet.hardware_addr_len, 6);
        assert_eq!(packet.protocol_addr_len, 4);
        assert_eq!(packet.operation, 1);
    }
}