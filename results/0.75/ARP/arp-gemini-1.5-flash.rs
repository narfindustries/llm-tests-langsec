use nom::{
    be_u16, be_u8, bytes::complete::take, combinator::map, IResult,
    number::complete::le_u32, sequence::tuple,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::net::Ipv4Addr;

#[derive(Debug)]
struct ArpPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_len: u8,
    protocol_len: u8,
    opcode: u16,
    sender_mac: [u8; 6],
    sender_ip: Ipv4Addr,
    target_mac: [u8; 6],
    target_ip: Ipv4Addr,
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (remaining, (hardware_type, protocol_type, hardware_len, protocol_len, opcode, sender_mac, sender_ip, target_mac, target_ip)) = tuple((
        be_u16,
        be_u16,
        be_u8,
        be_u8,
        be_u16,
        take(6usize),
        map(take(4usize), |ip_bytes: &[u8]| {
            Ipv4Addr::new(ip_bytes[0], ip_bytes[1], ip_bytes[2], ip_bytes[3])
        }),
        take(6usize),
        map(take(4usize), |ip_bytes: &[u8]| {
            Ipv4Addr::new(ip_bytes[0], ip_bytes[1], ip_bytes[2], ip_bytes[3])
        }),
    ))(input)?;

    Ok((
        remaining,
        ArpPacket {
            hardware_type,
            protocol_type,
            hardware_len,
            protocol_len,
            opcode,
            sender_mac: sender_mac.try_into().unwrap(),
            sender_ip,
            target_mac: target_mac.try_into().unwrap(),
            target_ip,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match parse_arp_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed ARP packet:\n{:?}", packet);
            if !remaining.is_empty() {
                println!("Remaining bytes: {:?}", remaining);
            }
        }
        Err(err) => {
            println!("Error parsing ARP packet: {:?}", err);
        }
    }
}

