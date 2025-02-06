use nom::{
    number::complete::{be_f32, be_i8, be_u32, be_u64, be_u8},
    sequence::tuple,
    IResult,
    bytes::complete::take,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct NTPv4Packet {
    leap_indicator: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    key_identifier: Option<u32>,
    message_digest: Option<Vec<u8>>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPv4Packet> {
    let (input, (li_vn_mode, stratum, poll, precision, root_delay, root_dispersion, reference_id, reference_timestamp, origin_timestamp, receive_timestamp, transmit_timestamp)) = tuple((
        be_u8, be_u8, be_i8, be_i8, be_f32, be_f32, be_u32, be_u64, be_u64, be_u64, be_u64,
    ))(input)?;

    let (input, key_identifier) = if input.is_empty() {
        (input, None)
    } else {
        let (input, key_id) = be_u32(input)?;
        (input, Some(key_id))
    };

    let (input, message_digest) = if input.is_empty() {
        (input, None)
    } else {
        let (input, digest) = take(16usize)(input)?;
        (input, Some(digest.to_vec()))
    };

    let (li, vn, mode) = parse_li_vn_mode(li_vn_mode);

    Ok((input, NTPv4Packet {
        leap_indicator: li,
        version: vn,
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
        key_identifier,
        message_digest,
    }))
}

fn parse_li_vn_mode(input: u8) -> (u8, u8, u8) {
    let li = (input >> 6) & 0b11;
    let vn = (input >> 3) & 0b111;
    let mode = input & 0b111;
    (li, vn, mode)
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