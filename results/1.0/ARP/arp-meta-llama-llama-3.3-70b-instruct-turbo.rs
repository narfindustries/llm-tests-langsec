use nom::{
    bytes::complete::{take},
    combinator::{map},
    number::complete::{be_u16, be_u8},
    sequence::{tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, BufReader},
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
    Autonet,
    LocalTalk,
    LocalNet,
    UltraLink,
    SmDS,
    FrameRelay,
    Atm,
    Hdlc,
    FibreChannel,
    SerialLine,
    Metricom,
    Ieee13941995,
    MAPOS,
    Twinaxial,
    Eui64,
    Hiparp,
    IpAndArpOverIso8473,
    IpxOverIso8473,
    IpOverInfiniband,
    Other(u16),
}

impl From<u16> for HardwareType {
    fn from(value: u16) -> Self {
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
            10 => HardwareType::Autonet,
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
            22 => HardwareType::Other(22),
            23 => HardwareType::Metricom,
            24 => HardwareType::Ieee13941995,
            25 => HardwareType::MAPOS,
            26 => HardwareType::Twinaxial,
            27 => HardwareType::Eui64,
            28 => HardwareType::Hiparp,
            29 => HardwareType::IpAndArpOverIso8473,
            30 => HardwareType::IpxOverIso8473,
            31 => HardwareType::IpOverInfiniband,
            _ => HardwareType::Other(value),
        }
    }
}

#[derive(Debug, PartialEq)]
enum ProtocolType {
    Ipv4,
    Arp,
    Rarp,
    Ipv6,
    Other(u16),
}

impl From<u16> for ProtocolType {
    fn from(value: u16) -> Self {
        match value {
            0x0800 => ProtocolType::Ipv4,
            0x0806 => ProtocolType::Arp,
            0x0835 => ProtocolType::Rarp,
            0x86DD => ProtocolType::Ipv6,
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
    RequestInverse,
    ReplyInverse,
    Other(u16),
}

impl From<u16> for Operation {
    fn from(value: u16) -> Self {
        match value {
            1 => Operation::Request,
            2 => Operation::Reply,
            3 => Operation::RequestReverse,
            4 => Operation::ReplyReverse,
            8 => Operation::RequestInverse,
            9 => Operation::ReplyInverse,
            _ => Operation::Other(value),
        }
    }
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

impl ArpPacket {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        map(
            tuple((
                map(be_u16::<&[u8], nom::error::Error<&[u8]>>, HardwareType::from),
                map(be_u16::<&[u8], nom::error::Error<&[u8]>>, ProtocolType::from),
                be_u8,
                be_u8,
                map(be_u16::<&[u8], nom::error::Error<&[u8]>>, Operation::from),
                take(6usize),
                take(4usize),
                take(6usize),
                take(4usize),
            )),
            |(hardware_type, protocol_type, hardware_address_length, protocol_address_length, operation, sender_hardware_address, sender_protocol_address, target_hardware_address, target_protocol_address)| {
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
                }
            },
        )(input)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }

    let file = File::open(&args[1]).expect("Could not open file");
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).expect(" bisa tidak read file");

    let result = ArpPacket::parse(&input);
    match result {
        Ok((_, packet)) => {
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
        }
        Err(err) => {
            panic!("Error parsing ARP packet: {:?}", err);
        }
    }
}