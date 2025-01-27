use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::take,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{tuple, preceded},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, stdin},
    path::Path,
};

#[derive(Debug)]
enum HardwareType {
    Ethernet,
    ExperimentalEthernet,
    Other(u16),
}

impl HardwareType {
    fn parse(input: &[u8]) -> IResult<&[u8], HardwareType> {
        map(be_u16, |n| match n {
            1 => HardwareType::Ethernet,
            2 => HardwareType::ExperimentalEthernet,
            n => HardwareType::Other(n),
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
            n => ProtocolType::Other(n),
        })(input)
    }
}

#[derive(Debug)]
enum Operation {
    Request,
    Reply,
    Other(u16),
}

impl Operation {
    fn parse(input: &[u8]) -> IResult<&[u8], Operation> {
        map(be_u16, |n| match n {
            1 => Operation::Request,
            2 => Operation::Reply,
            n => Operation::Other(n),
        })(input)
    }
}

#[derive(Debug)]
struct ArpPacket {
    hardware_type: HardwareType,
    protocol_type: ProtocolType,
    hardware_len: u8,
    protocol_len: u8,
    operation: Operation,
    sender_hardware_addr: Vec<u8>,
    sender_protocol_addr: Vec<u8>,
    target_hardware_addr: Vec<u8>,
    target_protocol_addr: Vec<u8>,
}

impl ArpPacket {
    fn parse(input: &[u8]) -> IResult<&[u8], ArpPacket> {
        map(
            tuple((
                HardwareType::parse,
                ProtocolType::parse,
                be_u8,
                be_u8,
                Operation::parse,
                take_opt,
                take_opt,
                take_opt,
                take_opt,
            )),
            |(
                hardware_type,
                protocol_type,
                hardware_len,
                protocol_len,
                operation,
                sender_hardware_addr,
                sender_protocol_addr,
                target_hardware_addr,
                target_protocol_addr,
            )| ArpPacket {
                hardware_type,
                protocol_type,
                hardware_len,
                protocol_len,
                operation,
                sender_hardware_addr.unwrap_or_default(),
                sender_protocol_addr.unwrap_or_default(),
                target_hardware_addr.unwrap_or_default(),
                target_protocol_addr.unwrap_or_default(),
            },
        )(input)
    }
}

fn take_opt<Input, Output>(input: Input) -> IResult<Input, Option<Output>>
where
    Input: nom::InputTake,
    Output: nom::InputTake,
{
    opt(take(1usize))(input)
}

fn main() {
    let filename = env::args().nth(1).expect("filename is required");
    let mut file = match File::open(&Path::new(&filename)) {
        Ok(f) => f,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };
    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(n) => {
            if n == 0 {
                eprintln!("File is empty.");
                return;
            }
        }
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    }
    match ArpPacket::parse(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(err) => eprintln!("Error parsing packet: {:?}", err),
    }
}