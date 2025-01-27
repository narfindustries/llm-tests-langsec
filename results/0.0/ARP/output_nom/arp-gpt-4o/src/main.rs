use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    number::complete::{be_u8, be_u16},
    bytes::complete::take,
};

#[derive(Debug)]
struct ArpPacket<'a> {
    htype: u16,
    ptype: u16,
    hlen: u8,
    plen: u8,
    oper: u16,
    sha: &'a [u8],
    spa: &'a [u8],
    tha: &'a [u8],
    tpa: &'a [u8],
}

fn parse_arp(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, htype) = be_u16(input)?;
    let (input, ptype) = be_u16(input)?;
    let (input, hlen) = be_u8(input)?;
    let (input, plen) = be_u8(input)?;
    let (input, oper) = be_u16(input)?;
    let (input, sha) = take(hlen)(input)?;
    let (input, spa) = take(plen)(input)?;
    let (input, tha) = take(hlen)(input)?;
    let (input, tpa) = take(plen)(input)?;

    Ok((input, ArpPacket { htype, ptype, hlen, plen, oper, sha, spa, tha, tpa }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_arp(&buffer) {
        Ok((_, arp_packet)) => {
            println!("{:?}", arp_packet);
        }
        Err(e) => {
            eprintln!("Failed to parse ARP packet: {:?}", e);
        }
    }
}