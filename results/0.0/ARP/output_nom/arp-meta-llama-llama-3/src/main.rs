use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::take,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Result},
};

#[derive(Debug)]
struct ArpHeader {
    hw_type: u16,
    proto_type: u16,
    hw_len: u8,
    proto_len: u8,
    op: u16,
}

#[derive(Debug)]
struct ArpPacket {
    header: ArpHeader,
    sender_hw_addr: [u8; 6],
    sender_proto_addr: [u8; 4],
    target_hw_addr: [u8; 6],
    target_proto_addr: [u8; 4],
}

fn parse_arp_header(input: &[u8]) -> IResult<&[u8], ArpHeader> {
    let (input, hw_type) = be_u16(input)?;
    let (input, proto_type) = be_u16(input)?;
    let (input, hw_len) = be_u8(input)?;
    let (input, proto_len) = be_u8(input)?;
    let (input, op) = be_u16(input)?;
    Ok((input, ArpHeader { hw_type, proto_type, hw_len, proto_len, op }))
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, header) = parse_arp_header(input)?;
    let (input, sender_hw_addr) = take(6u8)(input)?;
    let (input, sender_proto_addr) = take(4u8)(input)?;
    let (input, target_hw_addr) = take(6u8)(input)?;
    let (input, target_proto_addr) = take(4u8)(input)?;
    Ok((
        input,
        ArpPacket {
            header,
            sender_hw_addr: sender_hw_addr.try_into().unwrap(),
            sender_proto_addr: sender_proto_addr.try_into().unwrap(),
            target_hw_addr: target_hw_addr.try_into().unwrap(),
            target_proto_addr: target_proto_addr.try_into().unwrap(),
        },
    ))
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let mut file = File::open(&args[1])?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    let (_rest, packet) = parse_arp_packet(&data).unwrap();
    println!("{:?}", packet);
    Ok(())
}