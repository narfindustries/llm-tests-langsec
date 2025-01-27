use nom::{
    bits::complete::{tag, take},
    branch::alt,
    bytes::complete::take,
    combinator::map,
    error::ErrorKind,
    multi::length_data,
    number::complete::{be_u8, be_u16, be_u32, be_u64},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::Read,
};

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version: u8,
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
    extension: Option<Vec<u8>>,
    key_identifier: Option<u32>,
    message_digest: Option<[u8; 16]>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, (li_vn_mode, stratum, poll, precision)) = tuple((be_u8, be_u8, be_u8, be_u8))(input)?;
    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, origin_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let leap_indicator = (li_vn_mode >> 6) & 0b11;
    let version = (li_vn_mode >> 3) & 0b111;
    let mode = li_vn_mode & 0b111;

    let (input, extension) = if mode == 3 || mode == 4 {
        let (input, ext_len) = be_u16(input)?;
        let (input, ext_data) = take(ext_len as usize)(input)?;
        (input, Some(ext_data.to_vec()))
    } else {
        (input, None)
    };

    let (input, key_identifier) = if mode == 4 {
        let (input, key_id) = be_u32(input)?;
        (input, Some(key_id))
    } else {
        (input, None)
    };

    let (input, message_digest) = if mode == 4 {
        let (input, digest) = take(16usize)(input)?;
        (input, Some(digest.try_into().unwrap()))
    } else {
        (input, None)
    };

    Ok((
        input,
        NtpPacket {
            leap_indicator,
            version,
            mode,
            stratum,
            poll,
            precision,
            root_delay,
            root_dispersion,
            reference_id,
            reference_timestamp,
            origin_timestamp,
            receive_timestamp,
            transmit_timestamp,
            extension,
            key_identifier,
            message_digest,
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