use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct ARPHeader {
    hardware_type: u16,
    protocol_type: u16,
    hw_address_length: u8,
    proto_address_length: u8,
    operation: u16,
    sender_hw_address: Vec<u8>,
    sender_proto_address: Vec<u8>,
    target_hw_address: Vec<u8>,
    target_proto_address: Vec<u8>,
}

fn parse_arp_header(input: &[u8]) -> IResult<&[u8], ARPHeader> {
    let (input, hardware_type) = be_u16(input)?;
    let (input, protocol_type) = be_u16(input)?;
    let (input, hw_address_length) = be_u8(input)?;
    let (input, proto_address_length) = be_u8(input)?;
    let (input, operation) = be_u16(input)?;

    let hw_addr_len = hw_address_length as usize;
    let proto_addr_len = proto_address_length as usize;

    let (input, sender_hw_address) = take(hw_addr_len)(input)?;
    let (input, sender_proto_address) = take(proto_addr_len)(input)?;
    let (input, target_hw_address) = take(hw_addr_len)(input)?;
    let (input, target_proto_address) = take(proto_addr_len)(input)?;

    Ok((
        input,
        ARPHeader {
            hardware_type,
            protocol_type,
            hw_address_length,
            proto_address_length,
            operation,
            sender_hw_address: sender_hw_address.to_vec(),
            sender_proto_address: sender_proto_address.to_vec(),
            target_hw_address: target_hw_address.to_vec(),
            target_proto_address: target_proto_address.to_vec(),
        },
    ))
}

fn read_file(file_path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ARP_PACKET_FILE>", args[0]);
        std::process::exit(1);
    }

    let file_path = &args[1];
    match read_file(file_path) {
        Ok(bytes) => match parse_arp_header(&bytes) {
            Ok((_remaining, arp_header)) => {
                println!("{:#?}", arp_header);
            }
            Err(e) => {
                eprintln!("Failed to parse ARP header: {}", e);
            }
        },
        Err(e) => {
            eprintln!("Failed to read file: {}", e);
        }
    }
}