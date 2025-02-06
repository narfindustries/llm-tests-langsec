use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    number::complete::{be_u8, be_u32, be_f32, be_u64},
    sequence::tuple,
};

#[derive(Debug, PartialEq)]
struct NTPPacket {
    li: u8,
    vn: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_identifier: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    let (input, (flags, stratum, poll, precision, root_delay, root_dispersion, reference_identifier,
                 reference_timestamp, origin_timestamp, receive_timestamp, transmit_timestamp)) = tuple((
        be_u8,
        be_u8,
        be_i8,
        be_i8,
        be_f32,
        be_f32,
        be_u32,
        be_u64,
        be_u64,
        be_u64,
        be_u64,
    ))(input)?;

    let li = (flags >> 6) & 0x03;
    let vn = (flags >> 3) & 0x07;
    let mode = flags & 0x07;

    Ok((
        input,
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
        }
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_packet_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("{:#?}", packet);
        },
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {:?}", e);
        }
    }

    Ok(())
}