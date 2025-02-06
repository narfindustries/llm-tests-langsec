use nom::{
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct ARP {
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

fn parse_arp(input: &[u8]) -> IResult<&[u8], ARP> {
    let (input, htype) = be_u16(input)?;
    let (input, ptype) = be_u16(input)?;
    let (input, hlen) = be_u8(input)?;
    let (input, plen) = be_u8(input)?;
    let (input, oper) = be_u16(input)?;
    let (input, sha) = nom::bytes::complete::take(hlen as usize)(input)?;
    let (input, spa) = nom::bytes::complete::take(plen as usize)(input)?;
    let (input, tha) = nom::bytes::complete::take(hlen as usize)(input)?;
    let (input, tpa) = nom::bytes::complete::take(plen as usize)(input)?;

    Ok((
        input,
        ARP {
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
        println!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_arp(&buffer) {
        Ok((_remaining, arp)) => {
            println!("Parsed ARP Packet: {:?}", arp);
        }
        Err(e) => {
            println!("Failed to parse ARP packet: {:?}", e);
        }
    }

    Ok(())
}