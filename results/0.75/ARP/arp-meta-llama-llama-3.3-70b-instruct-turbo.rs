use nom::{bytes::complete::take, combinator::map, IResult};
use std::{env, fs};

#[derive(Debug, PartialEq)]
enum HardwareType {
    Ethernet,
    ExperimentalEthernet,
    AmateurRadioAX25,
    ProteonProNETTokenRing,
    Chaos,
    Ieee802Networks,
    Arcnet,
    Hyperchannel,
    Lanstar,
    AutonetShortAddress,
    LocalTalk,
    LocalNet,
    UltraLink,
    SmDS,
    FrameRelay,
    ATM,
    HDLC,
    FibreChannel,
    SerialLine,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum ProtocolType {
    Ipv4,
    Arp,
    Rarp,
    Trill,
    Ipv6,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum Operation {
    Request,
    Reply,
    RequestReverse,
    ReplyReverse,
    Other(u16),
}

#[derive(Debug, PartialEq)]
struct ArpPacket {
    hardware_type: HardwareType,
    protocol_type: ProtocolType,
    hardware_address_length: u8,
    protocol_address_length: u8,
    operation: Operation,
    sender_hardware_address: Vec<u8>,
    sender_protocol_address: Vec<u8>,
    target_hardware_address: Vec<u8>,
    target_protocol_address: Vec<u8>,
}

fn parse_hardware_type(input: &[u8]) -> IResult<&[u8], HardwareType> {
    map(take(2usize), |input: &[u8]| {
        let value = u16::from_be_bytes([input[0], input[1]]);
        match value {
            1 => HardwareType::Ethernet,
            2 => HardwareType::ExperimentalEthernet,
            3 => HardwareType::AmateurRadioAX25,
            4 => HardwareType::ProteonProNETTokenRing,
            5 => HardwareType::Chaos,
            6 => HardwareType::Ieee802Networks,
            7 => HardwareType::Arcnet,
            8 => HardwareType::Hyperchannel,
            9 => HardwareType::Lanstar,
            10 => HardwareType::AutonetShortAddress,
            11 => HardwareType::LocalTalk,
            12 => HardwareType::LocalNet,
            13 => HardwareType::UltraLink,
            14 => HardwareType::SmDS,
            15 => HardwareType::FrameRelay,
            16 => HardwareType::ATM,
            17 => HardwareType::HDLC,
            18 => HardwareType::FibreChannel,
            20 => HardwareType::SerialLine,
            _ => HardwareType::Other(value),
        }
    })(input)
}

fn parse_protocol_type(input: &[u8]) -> IResult<&[u8], ProtocolType> {
    map(take(2usize), |input: &[u8]| {
        let value = u16::from_be_bytes([input[0], input[1]]);
        match value {
            0x0800 => ProtocolType::Ipv4,
            0x0806 => ProtocolType::Arp,
            0x0835 => ProtocolType::Rarp,
            0x22F3 => ProtocolType::Trill,
            0x86DD => ProtocolType::Ipv6,
            _ => ProtocolType::Other(value),
        }
    })(input)
}

fn parse_hardware_address_length(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1usize), |input: &[u8]| input[0])(input)
}

fn parse_protocol_address_length(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1usize), |input: &[u8]| input[0])(input)
}

fn parse_operation(input: &[u8]) -> IResult<&[u8], Operation> {
    map(take(2usize), |input: &[u8]| {
        let value = u16::from_be_bytes([input[0], input[1]]);
        match value {
            1 => Operation::Request,
            2 => Operation::Reply,
            3 => Operation::RequestReverse,
            4 => Operation::ReplyReverse,
            _ => Operation::Other(value),
        }
    })(input)
}

fn parse_address(input: &[u8], length: u8) -> IResult<&[u8], Vec<u8>> {
    map(take(length as usize), |input: &[u8]| input.to_vec())(input)
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, hardware_type) = parse_hardware_type(input)?;
    let (input, protocol_type) = parse_protocol_type(input)?;
    let (input, hardware_address_length) = parse_hardware_address_length(input)?;
    let (input, protocol_address_length) = parse_protocol_address_length(input)?;
    let (input, operation) = parse_operation(input)?;
    let (input, sender_hardware_address) = parse_address(input, hardware_address_length)?;
    let (input, sender_protocol_address) = parse_address(input, protocol_address_length)?;
    let (input, target_hardware_address) = parse_address(input, hardware_address_length)?;
    let (input, target_protocol_address) = parse_address(input, protocol_address_length)?;

    Ok((input, ArpPacket {
        hardware_type,
        protocol_type,
        hardware_address_length,
        protocol_address_length,
        operation,
        sender_hardware_address,
        sender_protocol_address,
        target_hardware_address,
        target_protocol_address,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let input_file = &args[1];
    let input = fs::read(input_file).expect("Failed to read input file");

    match parse_arp_packet(&input) {
        Ok((remaining, packet)) => {
            println!("ARP Packet:");
            println!("  Hardware Type: {:?}", packet.hardware_type);
            println!("  Protocol Type: {:?}", packet.protocol_type);
            println!("  Hardware Address Length: {}", packet.hardware_address_length);
            println!("  Protocol Address Length: {}", packet.protocol_address_length);
            println!("  Operation: {:?}", packet.operation);
            println!("  Sender Hardware Address: {:?}", packet.sender_hardware_address);
            println!("  Sender Protocol Address: {:?}", packet.sender_protocol_address);
            println!("  Target Hardware Address: {:?}", packet.target_hardware_address);
            println!("  Target Protocol Address: {:?}", packet.target_protocol_address);

            if remaining.len() > 0 {
                println!("Warning: {} bytes remaining after parsing", remaining.len());
            }
        }
        Err(err) => {
            println!("Error parsing ARP packet: {}", err);
        }
    }
}