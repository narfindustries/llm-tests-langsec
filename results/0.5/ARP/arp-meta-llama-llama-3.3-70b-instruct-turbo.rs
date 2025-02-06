use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::map;
use nom::number::complete::{be_u16, be_u8};
use nom::sequence::tuple;
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum HardwareType {
    Ethernet,
    ExperimentalEthernet,
    AmateurRadioAX25,
    ProteonProNETTokenRing,
    Chaos,
    IEEE802Networks,
    ARCNET,
    Hyperchannel,
    Lanstar,
    AutonetShortAddress,
    LocalTalk,
    LocalNet,
    UltraLink,
    SMDS,
    FrameRelay,
    ATM,
    HDLC,
    FibreChannel,
    SerialLine,
    MILSTD188220,
    Metricom,
    IEEE13941995,
    MAPOS,
    Twinaxial,
    EUI64,
    HIPARP,
    IPandARPOverISO78161,
    ARPOverFibreChannelFC,
    IBMSNAOver8022,
    IBMSNAOver8022WithLLC1,
    Other(u16),
}

impl HardwareType {
    fn parse(i: &[u8]) -> IResult<&[u8], HardwareType> {
        map(be_u16, |n| match n {
            1 => HardwareType::Ethernet,
            2 => HardwareType::ExperimentalEthernet,
            3 => HardwareType::AmateurRadioAX25,
            4 => HardwareType::ProteonProNETTokenRing,
            5 => HardwareType::Chaos,
            6 => HardwareType::IEEE802Networks,
            7 => HardwareType::ARCNET,
            8 => HardwareType::Hyperchannel,
            9 => HardwareType::Lanstar,
            10 => HardwareType::AutonetShortAddress,
            11 => HardwareType::LocalTalk,
            12 => HardwareType::LocalNet,
            13 => HardwareType::UltraLink,
            14 => HardwareType::SMDS,
            15 => HardwareType::FrameRelay,
            16 => HardwareType::ATM,
            17 => HardwareType::HDLC,
            18 => HardwareType::FibreChannel,
            20 => HardwareType::SerialLine,
            22 => HardwareType::MILSTD188220,
            23 => HardwareType::Metricom,
            24 => HardwareType::IEEE13941995,
            25 => HardwareType::MAPOS,
            26 => HardwareType::Twinaxial,
            27 => HardwareType::EUI64,
            28 => HardwareType::HIPARP,
            29 => HardwareType::IPandARPOverISO78161,
            30 => HardwareType::ARPOverFibreChannelFC,
            31 => HardwareType::IBMSNAOver8022,
            32 => HardwareType::IBMSNAOver8022WithLLC1,
            _ => HardwareType::Other(n),
        })(i)
    }
}

#[derive(Debug, PartialEq)]
enum ProtocolType {
    IPv4,
    ARP,
    RARP,
    TRILL,
    Other(u16),
}

impl ProtocolType {
    fn parse(i: &[u8]) -> IResult<&[u8], ProtocolType> {
        map(be_u16, |n| match n {
            0x0800 => ProtocolType::IPv4,
            0x0806 => ProtocolType::ARP,
            0x0835 => ProtocolType::RARP,
            0x22F3 => ProtocolType::TRILL,
            _ => ProtocolType::Other(n),
        })(i)
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
    InARPRequest,
    InARPReply,
    Other(u16),
}

impl Operation {
    fn parse(i: &[u8]) -> IResult<&[u8], Operation> {
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
            _ => Operation::Other(n),
        })(i)
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

impl ARP {
    fn parse(i: &[u8]) -> IResult<&[u8], ARP> {
        tuple((
            HardwareType::parse,
            ProtocolType::parse,
            be_u8,
            be_u8,
            Operation::parse,
            take(6usize),
            take(4usize),
            take(6usize),
            take(4usize),
        ))(i)
        .map(|(i, (hardware_type, protocol_type, hardware_address_length, protocol_address_length, operation, sender_hardware_address, sender_protocol_address, target_hardware_address, target_protocol_address))| {
            (
                i,
                ARP {
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
            )
        })
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).unwrap();
    let mut data = Vec::new();
    file.read_to_end(&mut data).unwrap();

    match ARP::parse(&data) {
        Ok((_, arp)) => println!("{:?}", arp),
        Err(e) => println!("Error parsing ARP packet: {:?}", e),
    }
}