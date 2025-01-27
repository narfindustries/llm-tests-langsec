use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    number::complete::{be_u8, be_u16},
    bytes::complete::take,
};

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

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, htype) = be_u16(input)?;
    let (input, ptype) = be_u16(input)?;
    let (input, hlen) = be_u8(input)?;
    let (input, plen) = be_u8(input)?;
    let (input, oper) = be_u16(input)?;
    let (input, sha) = take(hlen)(input)?;
    let (input, spa) = take(plen)(input)?;
    let (input, tha) = take(hlen)(input)?;
    let (input, tpa) = take(plen)(input)?;

    Ok((input, ArpPacket {
        htype,
        ptype,
        hlen,
        plen,
        oper,
        sha: sha.to_vec(),
        spa: spa.to_vec(),
        tha: tha.to_vec(),
        tpa: tpa.to_vec(),
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_arp_packet(&buffer) {
        Ok((_, arp_packet)) => println!("{:?}", arp_packet),
        Err(e) => eprintln!("Failed to parse ARP packet: {:?}", e),
    }
}