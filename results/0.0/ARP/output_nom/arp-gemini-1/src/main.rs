use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ArpPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_len: u8,
    protocol_len: u8,
    opcode: u16,
    sender_mac: [u8; 6],
    sender_ip: u32,
    target_mac: [u8; 6],
    target_ip: u32,
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, hardware_type) = be_u16(input)?;
    let (input, protocol_type) = be_u16(input)?;
    let (input, hardware_len) = map(take(1usize), |x: &[u8]| x[0])(input)?;
    let (input, protocol_len) = map(take(1usize), |x: &[u8]| x[0])(input)?;
    let (input, opcode) = be_u16(input)?;
    let (input, sender_mac) = map(take(6usize), |x: &[u8]| {
        let mut mac = [0u8; 6];
        mac.copy_from_slice(x);
        mac
    })(input)?;
    let (input, sender_ip) = be_u32(input)?;
    let (input, target_mac) = map(take(6usize), |x: &[u8]| {
        let mut mac = [0u8; 6];
        mac.copy_from_slice(x);
        mac
    })(input)?;
    let (input, target_ip) = be_u32(input)?;

    Ok((
        input,
        ArpPacket {
            hardware_type,
            protocol_type,
            hardware_len,
            protocol_len,
            opcode,
            sender_mac,
            sender_ip,
            target_mac,
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
        Ok((_, packet)) => println!("{:?}", packet),
        Err(err) => println!("Error parsing ARP packet: {:?}", err),
    }
}
