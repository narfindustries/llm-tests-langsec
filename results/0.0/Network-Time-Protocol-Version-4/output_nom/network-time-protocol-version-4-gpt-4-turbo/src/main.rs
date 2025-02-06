use nom::{
    number::complete::{be_f64, be_u8, be_u32},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct NTPPacket {
    leap_indicator: u8,
    version_number: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: f64,
    root_dispersion: f64,
    reference_identifier: u32,
    reference_timestamp: f64,
    origin_timestamp: f64,
    receive_timestamp: f64,
    transmit_timestamp: f64,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    let (input, first_byte) = be_u8(input)?;
    let leap_indicator = first_byte >> 6;
    let version_number = (first_byte & 0x38) >> 3;
    let mode = first_byte & 0x07;

    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_u8(input)?;

    let (input, root_delay) = be_u32(input)?;
    let root_delay = root_delay as f64 / (1 << 16) as f64;

    let (input, root_dispersion) = be_u32(input)?;
    let root_dispersion = root_dispersion as f64 / (1 << 16) as f64;

    let (input, reference_identifier) = be_u32(input)?;

    let (input, reference_timestamp) = be_f64(input)?;
    let (input, origin_timestamp) = be_f64(input)?;
    let (input, receive_timestamp) = be_f64(input)?;
    let (input, transmit_timestamp) = be_f64(input)?;

    Ok((
        input,
        NTPPacket {
            leap_indicator,
            version_number,
            mode,
            stratum,
            poll: poll as i8,
            precision: precision as i8,
            root_delay,
            root_dispersion,
            reference_identifier,
            reference_timestamp,
            origin_timestamp,
            receive_timestamp,
            transmit_timestamp,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("{:#?}", packet);
        }
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {:?}", e);
        }
    }

    Ok(())
}