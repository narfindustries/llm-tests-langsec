use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    number::complete::{be_u16, be_u8},
    sequence::{tuple, preceded},
    IResult,
};

#[derive(Debug, PartialEq)]
struct MacAddress([u8; 6]);

#[derive(Debug, PartialEq)]
struct IpAddress([u8; 4]);

#[derive(Debug, PartialEq)]
struct ArpPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_address_length: u8,
    protocol_address_length: u8,
    operation: u16,
    sender_hardware_address: MacAddress,
    sender_protocol_address: IpAddress,
    target_hardware_address: Option<MacAddress>,
    target_protocol_address: IpAddress,
}

fn parse_mac_address(input: &[u8]) -> IResult<&[u8], MacAddress> {
    map(take(6usize), |bytes: &[u8]| {
        let mut mac = [0u8; 6];
        mac.copy_from_slice(bytes);
        MacAddress(mac)
    })(input)
}

fn parse_ip_address(input: &[u8]) -> IResult<&[u8], IpAddress> {
    map(take(4usize), |bytes: &[u8]| {
        let mut ip = [0u8; 4];
        ip.copy_from_slice(bytes);
        IpAddress(ip)
    })(input)
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    map(
        tuple((
            be_u16, // hardware type
            be_u16, // protocol type
            be_u8,  // hardware address length
            be_u8,  // protocol address length
            be_u16, // operation
            parse_mac_address, // sender hardware address
            parse_ip_address, // sender protocol address
            opt(parse_mac_address), // target hardware address
            parse_ip_address, // target protocol address
        )),
        |(
            hardware_type,
            protocol_type,
            hardware_address_length,
            protocol_address_length,
            operation,
            sender_hardware_address,
            sender_protocol_address,
            target_hardware_address,
            target_protocol_address,
        )| ArpPacket {
            hardware_type,
            protocol_type,
            hardware_address_length,
            protocol_address_length,
            operation,
            sender_hardware_address,
            sender_protocol_address,
            target_hardware_address,
            target_protocol_address,
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
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}