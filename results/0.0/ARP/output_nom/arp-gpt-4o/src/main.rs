use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_u16, u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

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

fn parse_arp(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, htype) = be_u16(input)?;
    let (input, ptype) = be_u16(input)?;
    let (input, hlen) = u8(input)?;
    let (input, plen) = u8(input)?;
    let (input, oper) = be_u16(input)?;
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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_arp(&buffer) {
        Ok((_, arp_packet)) => {
            println!("{:?}", arp_packet);
        }
        Err(e) => {
            eprintln!("Failed to parse ARP packet: {:?}", e);
        }
    }

    Ok(())
}