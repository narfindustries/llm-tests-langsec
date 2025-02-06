use nom::{
    number::complete::{be_f64, be_u8, be_u16, be_u32},
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
    poll: u8,
    precision: i8,
    root_delay: f64,
    root_dispersion: f64,
    reference_identifier: u32,
    reference_timestamp: f64,
    originate_timestamp: f64,
    receive_timestamp: f64,
    transmit_timestamp: f64,
    // Optional fields are not included in this basic structure
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    let (input, first_byte) = be_u8(input)?;
    let leap_indicator = first_byte >> 6;
    let version_number = (first_byte & 0b00111000) >> 3;
    let mode = first_byte & 0b00000111;

    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_u8(input)?;

    let (input, root_delay) = be_f64(input)?;
    let (input, root_dispersion) = be_f64(input)?;
    let (input, reference_identifier) = be_u32(input)?;

    let (input, reference_timestamp) = be_f64(input)?;
    let (input, originate_timestamp) = be_f64(input)?;
    let (input, receive_timestamp) = be_f64(input)?;
    let (input, transmit_timestamp) = be_f64(input)?;

    Ok((
        input,
        NTPPacket {
            leap_indicator,
            version_number,
            mode,
            stratum,
            poll,
            precision: precision as i8,
            root_delay,
            root_dispersion,
            reference_identifier,
            reference_timestamp,
            originate_timestamp,
            receive_timestamp,
            transmit_timestamp,
        },
    ))
}

fn read_file_to_vec(filename: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(filename)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    Ok(data)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let data = read_file_to_vec(filename)?;

    match parse_ntp_packet(&data) {
        Ok((_, packet)) => {
            println!("{:?}", packet);
        }
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {:?}", e);
        }
    }

    Ok(())
}