use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::Read,
};

#[derive(Debug)]
struct ARPPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_len: u8,
    protocol_len: u8,
    operation: u16,
    sender_hardware_addr: Vec<u8>,
    sender_protocol_addr: Vec<u8>,
    target_hardware_addr: Vec<u8>,
    target_protocol_addr: Vec<u8>,
}

fn parse_hardware_addr(input: &[u8], len: u8) -> IResult<&[u8], Vec<u8>> {
    take(len)(input)
}

fn parse_protocol_addr(input: &[u8], len: u8) -> IResult<&[u8], Vec<u8>> {
    take(len)(input)
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ARPPacket> {
    let (input, (hardware_type, protocol_type, hardware_len, protocol_len, operation)) =
        tuple((be_u16, be_u16, be_u8, be_u8, be_u16))(input)?;

    let (input, sender_hardware_addr) = parse_hardware_addr(input, hardware_len)?;
    let (input, sender_protocol_addr) = parse_protocol_addr(input, protocol_len)?;
    let (input, target_hardware_addr) = parse_hardware_addr(input, hardware_len)?;
    let (input, target_protocol_addr) = parse_protocol_addr(input, protocol_len)?;

    Ok((
        input,
        ARPPacket {
            hardware_type,
            protocol_type,
            hardware_len,
            protocol_len,
            operation,
            sender_hardware_addr,
            sender_protocol_addr,
            target_hardware_addr,
            target_protocol_addr,
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