use nom::{
    bytes::complete::{take, tag},
    combinator::{map, map_res},
    number::complete::{le_u16, le_u8},
    IResult,
};
use std::env;
use std::fs::read;

#[derive(Debug)]
struct ArpPacket {
    htype: u16,
    ptype: u16,
    hlen: u8,
    plen: u8,
    opcode: u16,
    sha: Vec<u8>,
    spa: Vec<u8>,
    tha: Vec<u8>,
    tpa: Vec<u8>,
}

fn parse_arp_packet(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, htype) = le_u16(input)?;
    let (input, ptype) = le_u16(input)?;
    let (input, hlen) = le_u8(input)?;
    let (input, plen) = le_u8(input)?;
    let (input, opcode) = le_u16(input)?;
    let (input, sha) = take(hlen as usize)(input)?;
    let (input, spa) = take(plen as usize)(input)?;
    let (input, tha) = take(hlen as usize)(input)?;
    let (input, tpa) = take(plen as usize)(input)?;

    Ok((
        input,
        ArpPacket {
            htype,
            ptype,
            hlen,
            plen,
            opcode,
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
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    match read(filename) {
        Ok(buffer) => {
            match parse_arp_packet(&buffer) {
                Ok((_, packet)) => println!("{:?}", packet),
                Err(e) => eprintln!("Error parsing ARP packet: {:?}", e),
            }
        }
        Err(e) => eprintln!("Error reading file: {}", e),
    }
}
