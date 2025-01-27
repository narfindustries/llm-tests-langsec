use nom::{
    number::complete::{be_u8, be_u32},
    IResult,
    bytes::complete::take,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct NtpPacket {
    li_vn_mode: u8,
    stratum: u8,
    poll: u8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_identifier: u32,
    reference_timestamp: (u32, u32),
    originate_timestamp: (u32, u32),
    receive_timestamp: (u32, u32),
    transmit_timestamp: (u32, u32),
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(error) => {
            eprintln!("Failed to open file {}: {}", filename, error);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(error) = file.read_to_end(&mut buffer) {
        eprintln!("Failed to read file {}: {}", filename, error);
        return;
    }

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(_) => eprintln!("Failed to parse NTP packet"),
    }
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = be_u8(input)?;
    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_i8(input)?;
    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_identifier) = be_u32(input)?;
    let (input, reference_timestamp) = parse_timestamp(input)?;
    let (input, originate_timestamp) = parse_timestamp(input)?;
    let (input, receive_timestamp) = parse_timestamp(input)?;
    let (input, transmit_timestamp) = parse_timestamp(input)?;

    Ok((input, NtpPacket {
        li_vn_mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_identifier,
        reference_timestamp,
        originate_timestamp,
        receive_timestamp,
        transmit_timestamp,
    }))
}

fn parse_timestamp(input: &[u8]) -> IResult<&[u8], (u32, u32)> {
    let (input, seconds) = be_u32(input)?;
    let (input, fraction) = be_u32(input)?;
    Ok((input, (seconds, fraction)))
}

fn be_i8(i: &[u8]) -> IResult<&[u8], i8> {
    let (i, res) = take(1usize)(i)?;
    Ok((i, res[0] as i8))
}