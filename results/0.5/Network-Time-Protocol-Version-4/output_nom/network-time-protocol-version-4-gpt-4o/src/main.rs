use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    number::complete::{be_u8, be_u16, be_u32, be_f32, be_f64},
    combinator::{map},
    sequence::tuple,
    bytes::complete::take,
};

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version_number: u8,
    mode: u8,
    stratum: u8,
    poll: u8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_identifier: [u8; 4],
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, (li_vn_mode, stratum, poll, precision)) = tuple((be_u8, be_u8, be_u8, be_u8))(input)?;
    let (input, root_delay) = be_f32(input)?;
    let (input, root_dispersion) = be_f32(input)?;
    let (input, reference_identifier) = map(take(4usize), |bytes: &[u8]| [bytes[0], bytes[1], bytes[2], bytes[3]])(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let leap_indicator = (li_vn_mode >> 6) & 0x03;
    let version_number = (li_vn_mode >> 3) & 0x07;
    let mode = li_vn_mode & 0x07;

    Ok((input, NtpPacket {
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
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("{:?}", packet);
        },
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {:?}", e);
        }
    }
}