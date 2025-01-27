use nom::{
    bytes::complete::{take, tag},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct ArpPacket {
    hardware_type: u16,
    protocol_type: u16,
    hardware_len: u8,
    protocol_len: u8,
    opcode: u16,
    sender_mac: [u8; 6],
    sender_ip: [u8; 4],
    target_mac: [u8; 6],
    target_ip: [u8; 4],
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    map(
        tuple((
            be_u16,
            be_u16,
            take(1u8),
            take(1u8),
            be_u16,
            take(6u8),
            take(4u8),
            take(6u8),
            take(4u8),
        )),
        |(hardware_type, protocol_type, hardware_len, protocol_len, opcode, sender_mac, sender_ip, target_mac, target_ip)| {
            ArpPacket {
                hardware_type,
                protocol_type,
                hardware_len: hardware_len[0],
                protocol_len: protocol_len[0],
                opcode,
                sender_mac: sender_mac.try_into().unwrap(),
                sender_ip: sender_ip.try_into().unwrap(),
                target_mac: target_mac.try_into().unwrap(),
                target_ip: target_ip.try_into().unwrap(),
            }
        },
    )(input)
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_arp_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed ARP packet:\n{:?}\nRemaining bytes: {:?}", packet, remaining);
        }
        Err(e) => {
            eprintln!("Failed to parse ARP packet: {:?}", e);
        }
    }
}
