use nom::{
    bytes::complete::{take},
    combinator::{map},
    error::{context, Error, ErrorKind},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
enum HardwareType {
    Ethernet,
    ExperimentalEthernet,
    AmateurRadio,
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
    SerialLine,
    Other(u16),
}

impl HardwareType {
    fn from_u16(value: u16) -> Self {
        match value {
            1 => HardwareType::Ethernet,
            2 => HardwareType::ExperimentalEthernet,
            3 => HardwareType::AmateurRadio,
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
            20 => HardwareType::SerialLine,
            _ => HardwareType::Other(value),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ProtocolType {
    IPv4,
    ARP,
    RARP,
    TRILL,
    IPv6,
    Other(u16),
}

impl ProtocolType {
    fn from_u16(value: u16) -> Self {
        match value {
            0x0800 => ProtocolType::IPv4,
            0x0806 => ProtocolType::ARP,
            0x0835 => ProtocolType::RARP,
            0x22F3 => ProtocolType::TRILL,
            0x86DD => ProtocolType::IPv6,
            _ => ProtocolType::Other(value),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Operation {
    Request,
    Reply,
    RequestReverse,
    ReplyReverse,
    DRARPRequest,
    DRARPReply,
    DRARPError,
    Other(u16),
}

impl Operation {
    fn from_u16(value: u16) -> Self {
        match value {
            1 => Operation::Request,
            2 => Operation::Reply,
            3 => Operation::RequestReverse,
            4 => Operation::ReplyReverse,
            8 => Operation::DRARPRequest,
            9 => Operation::DRARPReply,
            10 => Operation::DRARPError,
            _ => Operation::Other(value),
        }
    }
}

#[derive(Debug, PartialEq)]
struct ARP {
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
    context(
        "hardware type",
        map(be_u16, HardwareType::from_u16),
    )(input)
}

fn parse_protocol_type(input: &[u8]) -> IResult<&[u8], ProtocolType> {
    context(
        "protocol type",
        map(be_u16, ProtocolType::from_u16),
    )(input)
}

fn parse_hardware_address_length(input: &[u8]) -> IResult<&[u8], u8> {
    context("hardware address length", be_u8)(input)
}

fn parse_protocol_address_length(input: &[u8]) -> IResult<&[u8], u8> {
    context("protocol address length", be_u8)(input)
}

fn parse_operation(input: &[u8]) -> IResult<&[u8], Operation> {
    context("operation", map(be_u16, Operation::from_u16))(input)
}

fn parse_hardware_address(input: &[u8], length: u8) -> IResult<&[u8], Vec<u8>> {
    context(
        "hardware address",
        map(take(length as usize), |x: &[u8]| x.to_vec()),
    )(input)
}

fn parse_protocol_address(input: &[u8], length: u8) -> IResult<&[u8], Vec<u8>> {
    context(
        "protocol address",
        map(take(length as usize), |x: &[u8]| x.to_vec()),
    )(input)
}

fn parse_arp(input: &[u8]) -> IResult<&[u8], ARP> {
    context(
        "arp",
        map(
            tuple((
                parse_hardware_type,
                parse_protocol_type,
                parse_hardware_address_length,
                parse_protocol_address_length,
                parse_operation,
            )),
            |(hardware_type, protocol_type, hardware_address_length, protocol_address_length, operation)| ARP {
                hardware_type,
                protocol_type,
                hardware_address_length,
                protocol_address_length,
                operation,
                sender_hardware_address: vec![],
                sender_protocol_address: vec![],
                target_hardware_address: vec![],
                target_protocol_address: vec![],
            },
        ),
    )(input)
}

fn parse_arp_full(input: &[u8]) -> IResult<&[u8], ARP> {
    context(
        "arp full",
        map(
            tuple((
                parse_hardware_type,
                parse_protocol_type,
                parse_hardware_address_length,
                parse_protocol_address_length,
                parse_operation,
                |input| parse_hardware_address(input, 6),
                |input| parse_protocol_address(input, 4),
                |input| parse_hardware_address(input, 6),
                |input| parse_protocol_address(input, 4),
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
            )| ARP {
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
        ),
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();

    match parse_arp_full(&input) {
        Ok((remaining, arp)) => {
            println!("ARP packet: {:?}", arp);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        }
        Err(err) => {
            println!("Error parsing ARP packet: {:?}", err);
        }
    }
}