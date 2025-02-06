use nom::{
    number::complete::{be_f64, be_i8, be_u16, be_u32, be_u64, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct NTPPacket {
    li: u8,
    vn: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: f64,
    root_dispersion: f64,
    reference_identifier: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    optional_fields: Option<Vec<u8>>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    let (input, first_byte) = be_u8(input)?;
    let li = first_byte >> 6;
    let vn = (first_byte & 0b00111000) >> 3;
    let mode = first_byte & 0b00000111;

    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_i8(input)?;
    let (input, precision) = be_i8(input)?;

    let (input, root_delay) = be_u32(input)?;
    let root_delay = root_delay as f64 / (1 << 16) as f64;

    let (input, root_dispersion) = be_u32(input)?;
    let root_dispersion = root_dispersion as f64 / (1 << 16) as f64;

    let (input, reference_identifier) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, origin_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let (remaining, optional_fields) = if !input.is_empty() {
        (input, Some(input.to_vec()))
    } else {
        (&input[..], None)
    };

    Ok((
        remaining,
        NTPPacket {
            li,
            vn,
            mode,
            stratum,
            poll,
            precision,
            root_delay,
            root_dispersion,
            reference_identifier,
            reference_timestamp,
            origin_timestamp,
            receive_timestamp,
            transmit_timestamp,
            optional_fields,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file_path>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("{:#?}", packet);
        }
        Err(e) => {
            println!("Failed to parse NTP packet: {:?}", e);
        }
    }

    Ok(())
}