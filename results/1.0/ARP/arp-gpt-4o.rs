use nom::{
    bytes::complete::take,
    combinator::map,
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct ArpPacket {
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

fn parse_u16(input: &[u8]) -> IResult<&[u8], u16> {
    let (input, bytes) = take(2usize)(input)?;
    Ok((input, u16::from_be_bytes([bytes[0], bytes[1]])))
}

fn parse_u8(input: &[u8]) -> IResult<&[u8], u8> {
    let (input, byte) = take(1usize)(input)?;
    Ok((input, byte[0]))
}

fn parse_arp(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, (htype, ptype, hlen, plen, oper)) =
        tuple((parse_u16, parse_u16, parse_u8, parse_u8, parse_u16))(input)?;
    let (input, sha) = take(hlen)(input)?;
    let (input, spa) = take(plen)(input)?;
    let (input, tha) = take(hlen)(input)?;
    let (input, tpa) = take(plen)(input)?;

    Ok((
        input,
        ArpPacket {
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
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = fs::read(filename).expect("Unable to read file");

    match parse_arp(&data) {
        Ok((_, arp_packet)) => {
            println!("{:?}", arp_packet);
        }
        Err(e) => {
            eprintln!("Failed to parse ARP packet: {:?}", e);
        }
    }
}