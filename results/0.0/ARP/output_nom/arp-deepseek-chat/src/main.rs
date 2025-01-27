use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    fs::File,
    io::Read,
    env,
};

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

fn parse_mac(input: &[u8]) -> IResult<&[u8], [u8; 6]> {
    let (input, mac) = take(6usize)(input)?;
    let mut mac_array = [0u8; 6];
    mac_array.copy_from_slice(mac);
    Ok((input, mac_array))
}

fn parse_ip(input: &[u8]) -> IResult<&[u8], [u8; 4]> {
    let (input, ip) = take(4usize)(input)?;
    let mut ip_array = [0u8; 4];
    ip_array.copy_from_slice(ip);
    Ok((input, ip_array))
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ARPPacket> {
    let (input, (hardware_type, protocol_type, hardware_size, protocol_size, opcode)) =
        tuple((be_u16, be_u16, be_u8, be_u8, be_u16))(input)?;

    let (input, sender_mac) = parse_mac(input)?;
    let (input, sender_ip) = parse_ip(input)?;
    let (input, target_mac) = parse_mac(input)?;
    let (input, target_ip) = parse_ip(input)?;

    Ok((
        input,
        ARPPacket {
            hardware_type,
            protocol_type,
            hardware_size,
            protocol_size,
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
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_arp_packet(&buffer) {
        Ok((_, arp_packet)) => println!("{:#?}", arp_packet),
        Err(e) => eprintln!("Failed to parse ARP packet: {:?}", e),
    }
}