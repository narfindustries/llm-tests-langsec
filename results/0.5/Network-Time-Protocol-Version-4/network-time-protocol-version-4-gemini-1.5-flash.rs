use nom::{
    be_u16, be_u32, be_u8, bytes::complete::take, combinator::map, IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NtpTimestamp {
    seconds: u32,
    fraction: u32,
}

#[derive(Debug)]
struct NtpPacket {
    li_vn_mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_id: u32,
    reference_timestamp: NtpTimestamp,
    originate_timestamp: NtpTimestamp,
    receive_timestamp: NtpTimestamp,
    transmit_timestamp: NtpTimestamp,
}


fn parse_ntp_timestamp(input: &[u8]) -> IResult<&[u8], NtpTimestamp> {
    map(
        pair(be_u32, be_u32),
        |(seconds, fraction)| NtpTimestamp { seconds, fraction },
    )(input)
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    map(
        tuple((
            be_u8,
            be_u8,
            be_i8,
            be_i8,
            be_u32,
            be_u32,
            be_u32,
            parse_ntp_timestamp,
            parse_ntp_timestamp,
            parse_ntp_timestamp,
            parse_ntp_timestamp,
        )),
        |(li_vn_mode, stratum, poll, precision, root_delay, root_dispersion, reference_id, reference_timestamp, originate_timestamp, receive_timestamp, transmit_timestamp)| {
            NtpPacket {
                li_vn_mode,
                stratum,
                poll,
                precision,
                root_delay,
                root_dispersion,
                reference_id,
                reference_timestamp,
                originate_timestamp,
                receive_timestamp,
                transmit_timestamp,
            }
        },
    )(input)
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(err) => println!("Error parsing NTP packet: {:?}", err),
    }
}

use nom::{pair, tuple};
