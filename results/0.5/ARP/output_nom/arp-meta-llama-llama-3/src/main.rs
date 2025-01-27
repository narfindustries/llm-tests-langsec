use nom::{
    bytes::complete::{take, take_till},
    combinator::{map, map_res},
    multi::take_while_m_n,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, stdin},
    path::Path,
};

#[derive(Debug)]
struct EthernetHeader {
    dst_mac: [u8; 6],
    src_mac: [u8; 6],
    ether_type: u16,
}

impl EthernetHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, dst_mac) = take(6u8)(input)?;
        let (input, src_mac) = take(6u8)(input)?;
        let (input, ether_type) = be_u16(input)?;
        Ok((
            input,
            EthernetHeader {
                dst_mac: dst_mac.try_into().unwrap(),
                src_mac: src_mac.try_into().unwrap(),
                ether_type,
            },
        ))
    }
}

#[derive(Debug)]
struct ArpHeader {
    hw_type: u16,
    proto_type: u16,
    hw_len: u8,
    proto_len: u8,
    op: u16,
    src_hw_addr: [u8; 6],
    src_ip_addr: [u8; 4],
    dst_hw_addr: [u8; 6],
    dst_ip_addr: [u8; 4],
}

impl ArpHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, hw_type) = be_u16(input)?;
        let (input, proto_type) = be_u16(input)?;
        let (input, hw_len) = be_u8(input)?;
        let (input, proto_len) = be_u8(input)?;
        let (input, op) = be_u16(input)?;
        let (input, src_hw_addr) = take(6u8)(input)?;
        let (input, src_ip_addr) = take(4u8)(input)?;
        let (input, dst_hw_addr) = take(6u8)(input)?;
        let (input, dst_ip_addr) = take(4u8)(input)?;
        Ok((
            input,
            ArpHeader {
                hw_type,
                proto_type,
                hw_len,
                proto_len,
                op,
                src_hw_addr: src_hw_addr.try_into().unwrap(),
                src_ip_addr: src_ip_addr.try_into().unwrap(),
                dst_hw_addr: dst_hw_addr.try_into().unwrap(),
                dst_ip_addr: dst_ip_addr.try_into().unwrap(),
            },
        ))
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let input = if args.len() > 1 {
        let path = Path::new(&args[1]);
        let mut file = File::open(path)?;
        let mut buffer = Vec::new();
        file.read_to_end(&mut buffer)?;
        buffer
    } else {
        let mut buffer = Vec::new();
        stdin().read_to_end(&mut buffer)?;
        buffer
    };

    let (_remaining, eth_header) = EthernetHeader::parse(&input).unwrap();
    let (_remaining, arp_header) = ArpHeader::parse(&input[eth_header.ether_type as usize..]).unwrap();

    println!("{:?}", eth_header);
    println!("{:?}", arp_header);

    Ok(())
}