use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::combinator::{map, opt};
use nom::multi::take_while_m_n;
use nom::number::complete::{be_u16, be_u8};
use nom::sequence::tuple;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum ARPHardwareType {
    Ethernet = 1,
}

#[derive(Debug, PartialEq)]
enum ARPProtocolType {
    IPv4 = 0x0800,
}

#[derive(Debug, PartialEq)]
enum ARPOperation {
    Request = 1,
    Reply = 2,
}

#[derive(Debug, PartialEq)]
struct ARPHeader {
    hardware_type: ARPHardwareType,
    protocol_type: ARPProtocolType,
    hardware_address_length: u8,
    protocol_address_length: u8,
    operation: ARPOperation,
    sender_hardware_address: Vec<u8>,
    sender_protocol_address: Vec<u8>,
    target_hardware_address: Vec<u8>,
    target_protocol_address: Vec<u8>,
}

fn parse_arp_hardware_type(input: &[u8]) -> nom::IResult<&[u8], ARPHardwareType> {
    map(be_u16, |n| match n {
        1 => ARPHardwareType::Ethernet,
        _ => panic!("Unsupported hardware type"),
    })(input)
}

fn parse_arp_protocol_type(input: &[u8]) -> nom::IResult<&[u8], ARPProtocolType> {
    map(be_u16, |n| match n {
        0x0800 => ARPProtocolType::IPv4,
        _ => panic!("Unsupported protocol type"),
    })(input)
}

fn parse_arp_operation(input: &[u8]) -> nom::IResult<&[u8], ARPOperation> {
    map(be_u16, |n| match n {
        1 => ARPOperation::Request,
        2 => ARPOperation::Reply,
        _ => panic!("Unsupported operation"),
    })(input)
}

fn parse_arp_header(input: &[u8]) -> nom::IResult<&[u8], ARPHeader> {
    map(
        tuple((
            parse_arp_hardware_type,
            parse_arp_protocol_type,
            be_u8,
            be_u8,
            parse_arp_operation,
            take(6), // sender hardware address
            take(4), // sender protocol address
            take(6), // target hardware address
            take(4), // target protocol address
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
        )| ARPHeader {
            hardware_type,
            protocol_type,
            hardware_address_length,
            protocol_address_length,
            operation,
            sender_hardware_address.to_vec(),
            sender_protocol_address.to_vec(),
            target_hardware_address.to_vec(),
            target_protocol_address.to_vec(),
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read file");

    match parse_arp_header(&data) {
        Ok((remaining, header)) => {
            println!("ARP Header: {:?}", header);
            if remaining.len() > 0 {
                println!("Remaining data: {:?}", remaining);
            }
        }
        Err(err) => {
            println!("Error parsing ARP header: {:?}", err);
        }
    }
}