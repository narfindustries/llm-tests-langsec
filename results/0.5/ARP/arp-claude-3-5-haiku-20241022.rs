use nom::{
    bytes::complete::{tag, take},
    multi::many0,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ARPPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_size: u8,
    protocol_size: u8,
    opcode: u16,
    sender_mac: [u8; 6],
    sender_ip: [u8; 4],
    target_mac: [u8; 6],
    target_ip: [u8; 4],
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ARPPacket> {
    let (input, (
        hardware_type,
        protocol_type,
        hardware_size,
        protocol_size,
        opcode,
        sender_mac,
        sender_ip,
        target_mac,
        target_ip
    )) = tuple((
        be_u16,
        be_u16,
        take(1usize),
        take(1usize),
        be_u16,
        take(6usize),
        take(4usize),
        take(6usize),
        take(4usize)
    ))(input)?;

    Ok((input, ARPPacket {
        hardware_type,
        protocol_type,
        hardware_size: hardware_size[0],
        protocol_size: protocol_size[0],
        opcode,
        sender_mac: sender_mac.try_into().unwrap(),
        sender_ip: sender_ip.try_into().unwrap(),
        target_mac: target_mac.try_into().unwrap(),
        target_ip: target_ip.try_into().unwrap(),
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
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}