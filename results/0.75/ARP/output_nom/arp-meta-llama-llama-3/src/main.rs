use nom::{
    bytes::complete::{take, tag},
    combinator::{map, verify},
    multi::take_while_m_n,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, stderr, Write};

#[derive(Debug)]
enum HardwareType {
    Ethernet,
    ExperimentalEthernet,
    Ax25,
    ProteonProNET,
    Chaos,
    IEEE802,
    Arcnet,
    Hyperchannel,
    Lanstar,
    AutonetShortAddress,
    LocalTalk,
    LocalNet,
    UltraLink,
    Smile,
    Fiber DistributedDataInterface,
    Other(u16),
}

impl HardwareType {
    fn parse(input: &[u8]) -> IResult<&[u8], HardwareType> {
        map(be_u16, |n| match n {
            1 => HardwareType::Ethernet,
            2 => HardwareType::ExperimentalEthernet,
            3 => HardwareType::Ax25,
            4 => HardwareType::ProteonProNET,
            5 => HardwareType::Chaos,
            6 => HardwareType::IEEE802,
            7 => HardwareType::Arcnet,
            8 => HardwareType::Hyperchannel,
            9 => HardwareType::Lanstar,
            10 => HardwareType::AutonetShortAddress,
            11 => HardwareType::LocalTalk,
            12 => HardwareType::LocalNet,
            13 => HardwareType::UltraLink,
            14 => HardwareType::Smile,
            15 => HardwareType::FiberDistributedDataInterface,
            _ => HardwareType::Other(n),
        })(input)
    }
}

#[derive(Debug)]
enum ProtocolType {
    IPv4,
    IPv6,
    Other(u16),
}

impl ProtocolType {
    fn parse(input: &[u8]) -> IResult<&[u8], ProtocolType> {
        map(be_u16, |n| match n {
            0x0800 => ProtocolType::IPv4,
            0x86dd => ProtocolType::IPv6,
            _ => ProtocolType::Other(n),
        })(input)
    }
}

#[derive(Debug)]
enum Operation {
    Request,
    Reply,
    RequestReverse,
    ReplyReverse,
    DRARPRequest,
    DRARPReply,
    DRARPError,
    InARPRequest,
    InARPReply,
    ARPNAK,
    M ARPRequest,
    MARPReply,
    Other(u16),
}

impl Operation {
    fn parse(input: &[u8]) -> IResult<&[u8], Operation> {
        map(be_u16, |n| match n {
            1 => Operation::Request,
            2 => Operation::Reply,
            3 => Operation::RequestReverse,
            4 => Operation::ReplyReverse,
            5 => Operation::DRARPRequest,
            6 => Operation::DRARPReply,
            7 => Operation::DRARPError,
            8 => Operation::InARPRequest,
            9 => Operation::InARPReply,
            10 => Operation::ARPNAK,
            11 => Operation::MARPRequest,
            12 => Operation::MARPReply,
            _ => Operation::Other(n),
        })(input)
    }
}

#[derive(Debug)]
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

impl ArpPacket {
    fn parse(input: &[u8]) -> IResult<&[u8], ArpPacket> {
        let (input, hardware_type) = HardwareType::parse(input)?;
        let (input, protocol_type) = ProtocolType::parse(input)?;
        let (input, hardware_address_length) = be_u8(input)?;
        let (input, protocol_address_length) = be_u8(input)?;
        let (input, operation) = Operation::parse(input)?;
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
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        writeln!(stderr(), "Usage: {} <file>", args[0]).unwrap();
        return;
    }
    let mut file = File::open(&args[1]).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();
    match ArpPacket::parse(&data) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => writeln!(stderr(), "Error: {}", e).unwrap(),
    }
}