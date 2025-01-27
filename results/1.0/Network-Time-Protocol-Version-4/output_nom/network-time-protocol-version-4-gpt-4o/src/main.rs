use nom::{
    bytes::complete::take,
    number::complete::{be_f32, be_u32, be_u8},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll: u8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: [u8; 4],
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    // Optional extensions could be added here if needed
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = be_u8(input)?;
    let leap_indicator = (li_vn_mode & 0b11000000) >> 6;
    let version = (li_vn_mode & 0b00111000) >> 3;
    let mode = li_vn_mode & 0b00000111;
    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_u8(input)? as i8;
    let (input, root_delay) = be_f32(input)?;
    let (input, root_dispersion) = be_f32(input)?;
    let (input, reference_id) = take(4usize)(input)?;
    let reference_id: [u8; 4] = reference_id.try_into().expect("slice with incorrect length");
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, origin_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

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
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <path to NTP binary file>", args[0]);
        return Ok(());
    }

    let path = &args[1];
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_, ntp_packet)) => {
            println!("{:?}", ntp_packet);
        }
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {:?}", e);
        }
    }

    Ok(())
}