use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    bytes::streaming::{tag, take},
    error::ErrorKind,
    multi::many0,
    number::streaming::{be_f64, be_u16, be_u32, be_u64, be_u8},
    sequence::tuple,
    IResult, Parser,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll_interval: i8,
    precision: i8,
    root_delay: f64,
    root_dispersion: f64,
    reference_id: Vec<u8>,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, (
        first_byte,
        stratum,
        poll_interval,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp
    )) = tuple((
        be_u8,
        be_u8,
        be_u8,
        be_u8,
        be_f64,
        be_f64,
        take(4usize),
        be_u64,
        be_u64,
        be_u64,
        be_u64
    ))(input)?;

    let leap_indicator = (first_byte & 0b11000000) >> 6;
    let version = (first_byte & 0b00111000) >> 3;
    let mode = first_byte & 0b00000111;

    Ok((input, NtpPacket {
        leap_indicator,
        version,
        mode,
        stratum,
        poll_interval: poll_interval as i8,
        precision: precision as i8,
        root_delay,
        root_dispersion,
        reference_id: reference_id.to_vec(),
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_file>", args[0]);
        std::process::exit(1);
    }

    let file_contents = fs::read(&args[1])?;
    match parse_ntp_packet(&file_contents) {
        Ok((_, packet)) => {
            println!("NTP Packet: {:?}", packet);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}