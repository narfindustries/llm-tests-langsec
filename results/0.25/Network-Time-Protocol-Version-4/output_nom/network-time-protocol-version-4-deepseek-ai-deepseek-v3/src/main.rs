use nom::{
    bytes::complete::take,
    number::complete::{be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version_number: u8,
    mode: u8,
    stratum: u8,
    poll: u8,
    precision: u8,
    root_delay: u32,
    root_dispersion: u32,
    reference_id: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    key_identifier: Option<u32>,
    mac: Option<Vec<u8>>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = take(1u8)(input)?;
    let (input, stratum) = take(1u8)(input)?;
    let (input, poll) = take(1u8)(input)?;
    let (input, precision) = take(1u8)(input)?;
    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, origin_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let (input, key_identifier) = if input.len() >= 4 {
        let (input, key_id) = be_u32(input)?;
        (input, Some(key_id))
    } else {
        (input, None)
    };

    let (input, mac) = if input.len() >= 16 {
        let (input, mac_bytes) = take(16usize)(input)?;
        (input, Some(mac_bytes.to_vec()))
    } else {
        (input, None)
    };

    let leap_indicator = (li_vn_mode[0] >> 6) & 0b11;
    let version_number = (li_vn_mode[0] >> 3) & 0b111;
    let mode = li_vn_mode[0] & 0b111;

    Ok((
        input,
        NtpPacket {
            leap_indicator,
            version_number,
            mode,
            stratum: stratum[0],
            poll: poll[0],
            precision: precision[0],
            root_delay,
            root_dispersion,
            reference_id,
            reference_timestamp,
            origin_timestamp,
            receive_timestamp,
            transmit_timestamp,
            key_identifier,
            mac,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse NTP packet: {:?}", e),
    }
}