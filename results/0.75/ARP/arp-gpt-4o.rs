use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    IResult,
    number::complete::{be_u16, be_u8},
    bytes::complete::take
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

fn parse_arp(input: &[u8]) -> IResult<&[u8], ArpPacket> {
    let (input, htype) = be_u16(input)?;
    let (input, ptype) = be_u16(input)?;
    let (input, hlen) = be_u8(input)?;
    let (input, plen) = be_u8(input)?;
    let (input, oper) = be_u16(input)?;
    
    let sha_len = hlen as usize;
    let spa_len = plen as usize;
    let (input, sha) = take(sha_len)(input)?;
    let (input, spa) = take(spa_len)(input)?;
    let (input, tha) = take(sha_len)(input)?;
    let (input, tpa) = take(spa_len)(input)?;

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
        eprintln!("Usage: {} <input file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_arp(&buffer) {
        Ok((_, packet)) => {
            println!("{:?}", packet);
        }
        Err(err) => {
            eprintln!("Failed to parse ARP packet: {:?}", err);
        }
    }
}