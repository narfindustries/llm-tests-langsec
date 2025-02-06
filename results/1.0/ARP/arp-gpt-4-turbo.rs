use nom::{
    bytes::complete::{take},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{env, fs::File, io::Read, fmt};

const ETHERNET_HW_TYPE: u16 = 1;

#[derive(Debug)]
struct ARP {
    htype: u16,
    ptype: u16,
    hlen: u8,
    plen: u8,
    operation: u16,
    sha: [u8; 6],
    spa: [u8; 4],
    tha: [u8; 6],
    tpa: [u8; 4],
}

impl fmt::Display for ARP {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "ARP Packet:
  Hardware Type: {}
  Protocol Type: {:04X}
  Hardware Address Length: {}
  Protocol Address Length: {}
  Operation: {}
  Sender Hardware Address: {:02X?}
  Sender Protocol Address: {}.{}.{}.{}
  Target Hardware Address: {:02X?}
  Target Protocol Address: {}.{}.{}.{}",
            self.htype,
            self.ptype,
            self.hlen,
            self.plen,
            self.operation,
            self.sha,
            self.spa[0],
            self.spa[1],
            self.spa[2],
            self.spa[3],
            self.tha,
            self.tpa[0],
            self.tpa[1],
            self.tpa[2],
            self.tpa[3]
        )
    }
}

fn parse_arp(input: &[u8]) -> IResult<&[u8], ARP> {
    let (input, htype) = be_u16(input)?;
    let (input, ptype) = be_u16(input)?;
    let (input, hlen) = be_u8(input)?;
    let (input, plen) = be_u8(input)?;
    let (input, operation) = be_u16(input)?;
    let (input, sha) = take(6usize)(input)?;
    let (input, spa) = take(4usize)(input)?;
    let (input, tha) = take(6usize)(input)?;
    let (input, tpa) = take(4usize)(input)?;

    let sha = <[u8; 6]>::try_from(sha).expect("Incorrect SHA length");
    let spa = <[u8; 4]>::try_from(spa).expect("Incorrect SPA length");
    let tha = <[u8; 6]>::try_from(tha).expect("Incorrect THA length");
    let tpa = <[u8; 4]>::try_from(tpa).expect("Incorrect TPA length");

    Ok((
        input,
        ARP {
            htype,
            ptype,
            hlen,
            plen,
            operation,
            sha,
            spa,
            tha,
            tpa,
        },
    ))
}

fn main() -> Result<(), std::io::Error> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_arp(&buffer) {
        Ok((_, arp)) => {
            println!("{}", arp);
        }
        Err(error) => {
            println!("Failed to parse ARP packet: {:?}", error);
        }
    }

    Ok(())
}