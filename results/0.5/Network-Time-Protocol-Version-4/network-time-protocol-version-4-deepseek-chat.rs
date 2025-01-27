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
    fs::File,
    io::Read,
    env,
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
    extension_fields: Option<Vec<u8>>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, (li_vn_mode, stratum, poll, precision, root_delay, root_dispersion, reference_id, reference_timestamp, origin_timestamp, receive_timestamp, transmit_timestamp)) = tuple((
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_u32,
        be_u32,
        be_u32,
        be_u64,
        be_u64,
        be_u64,
        be_u64,
    ))(input)?;

    let leap_indicator = (li_vn_mode >> 6) & 0b11;
    let version = (li_vn_mode >> 3) & 0b111;
    let mode = li_vn_mode & 0b111;

    let (input, extension_fields) = if input.len() > 0 {
        let (input, ext) = length_data(be_u16)(input)?;
        (input, Some(ext.to_vec()))
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
            extension_fields,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
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