use nom::{
    bits::complete::take as take_bits,
    bytes::complete::take,
    combinator::map,
    number::complete::{be_u8, be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::{fs, env};

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version_number: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_identifier: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extension_fields: Option<Vec<u8>>,
    key_identifier: Option<u32>,
    message_digest: Option<Vec<u8>>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, (li_vn_mode, stratum, poll, precision, root_delay, root_dispersion, reference_identifier, reference_timestamp, origin_timestamp, receive_timestamp, transmit_timestamp)) = tuple((
        be_u8, be_u8, be_u8, be_u8, be_u32, be_u32, be_u32, be_u64, be_u64, be_u64, be_u64,
    ))(input)?;

    let leap_indicator = li_vn_mode >> 6;
    let version_number = (li_vn_mode >> 3) & 0x07;
    let mode = li_vn_mode & 0x07;

    let (input, extension_fields) = if !input.is_empty() {
        let (input, fields) = take(input.len())(input)?;
        (input, Some(fields.to_vec()))
    } else {
        (input, None)
    };

    let (input, key_identifier) = if input.len() >= 4 {
        let (input, key_id) = be_u32(input)?;
        (input, Some(key_id))
    } else {
        (input, None)
    };

    let (input, message_digest) = if !input.is_empty() {
        let (input, digest) = take(input.len())(input)?;
        (input, Some(digest.to_vec()))
    } else {
        (input, None)
    };

    Ok((input, NtpPacket {
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
        extension_fields,
        key_identifier,
        message_digest,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    match parse_ntp_packet(&data) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse NTP packet: {:?}", e),
    }
}