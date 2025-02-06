use nom::{
    bytes::complete::{take},
    combinator::{map},
    error::{Error, ErrorKind},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Result},
};

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
    Atm,
    Hdlc,
    FibreChannel,
    Metricom,
    Ieee13941995,
    Mapos,
    Twinaxial,
    Eui64,
    Hiparp,
    IpAndArpOverIso78163,
    ArpOverFibreChannelFc,
    IbmSystemNetworkArchitectureSna,
    SerialLine,
    MilStd188220,
    Other(u16),
}

#[derive(Debug, PartialEq)]
enum ProtocolType {
    IPv4,
    Arp,
    Rarp,
    Trill,
    IPv6,
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
    map(be_u16, |n| match n {
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
        16 => HardwareType::Atm,
        17 => HardwareType::Hdlc,
        18 => HardwareType::FibreChannel,
        19 => HardwareType::Atm,
        20 => HardwareType::SerialLine,
        21 => HardwareType::Atm,
        22 => HardwareType::MilStd188220,
        23 => HardwareType::Metricom,
        24 => HardwareType::Ieee13941995,
        25 => HardwareType::Mapos,
        26 => HardwareType::Twinaxial,
        27 => HardwareType::Eui64,
        28 => HardwareType::Hiparp,
        29 => HardwareType::IpAndArpOverIso78163,
        30 => HardwareType::ArpOverFibreChannelFc,
        31 => HardwareType::IbmSystemNetworkArchitectureSna,
        _ => HardwareType::Other(n),
    })(input)
}

fn parse_protocol_type(input: &[u8]) -> IResult<&[u8], ProtocolType> {
    map(be_u16, |n| match n {
        0x0800 => ProtocolType::IPv4,
        0x0806 => ProtocolType::Arp,
        0x0835 => ProtocolType::Rarp,
        0x22F3 => ProtocolType::Trill,
        0x86DD => ProtocolType::IPv6,
        _ => ProtocolType::Other(n),
    })(input)
}

fn parse_operation(input: &[u8]) -> IResult<&[u8], Operation> {
    map(be_u16, |n| match n {
        1 => Operation::Request,
        2 => Operation::Reply,
        3 => Operation::RequestReverse,
        4 => Operation::ReplyReverse,
        _ => Operation::Other(n),
    })(input)
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, hardware_type) = parse_hardware_type(input)?;
    let (input, protocol_type) = parse_protocol_type(input)?;
    let (input, hardware_address_length) = be_u8(input)?;
    let (input, protocol_address_length) = be_u8(input)?;
    let (input, operation) = parse_operation(input)?;
    let (input, sender_hardware_address) = take(hardware_address_length as usize)(input)?;
    let (input, sender_protocol_address) = take(protocol_address_length as usize)(input)?;
    let (input, target_hardware_address) = take(hardware_address_length as usize)(input)?;
    let (input, target_protocol_address) = take(protocol_address_length as usize)(input)?;
    Ok((
        input,
        ArpPacket {
            hardware_type,
            protocol_type,
            hardware_address_length,
            protocol_address_length,
            operation,
            sender_hardware_address: sender_hardware_address.to_vec(),
            sender_protocol_address: sender_protocol_address.to_vec(),
            target_hardware_address: target_hardware_address.to_vec(),
            target_protocol_address: target_protocol_address.to_vec(),
        },
    ))
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let mut file = File::open(&args[1])?;
    let mut input = Vec::new();
    file.read_to_end(&mut input)?;
    match parse_arp_packet(&input) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(err) => println!("Error: {:?}", err),
    }
    Ok(())
}