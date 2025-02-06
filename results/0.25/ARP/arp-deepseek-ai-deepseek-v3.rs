use nom::{
    bytes::complete::take,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct ARPPacket {
    htype: u16,
    ptype: u16,
    hlen: u8,
    plen: u8,
    oper: u16,
    sha: Vec<u8>,
    spa: Vec<u8>,
    tha: Vec<u8>,
    tpa: Vec<u8>,
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ARPPacket> {
    let (input, htype) = be_u16(input)?;
    let (input, ptype) = be_u16(input)?;
    let (input, hlen) = be_u8(input)?;
    let (input, plen) = be_u8(input)?;
    let (input, oper) = be_u16(input)?;
    let (input, sha) = take(hlen as usize)(input)?;
    let (input, spa) = take(plen as usize)(input)?;
    let (input, tha) = take(hlen as usize)(input)?;
    let (input, tpa) = take(plen as usize)(input)?;

    Ok((
        input,
        ARPPacket {
            htype,
            ptype,
            hlen,
            plen,
            oper,
            sha: sha.to_vec(),
            spa: spa.to_vec(),
            tha: tha.to_vec(),
            tpa: tpa.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_arp_packet(&data) {
        Ok((_, arp_packet)) => println!("{:?}", arp_packet),
        Err(e) => eprintln!("Failed to parse ARP packet: {:?}", e),
    }
}