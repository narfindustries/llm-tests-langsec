use nom::{
    be::{be_u16, be_u32, be_u8},
    bytes::complete::take,
    combinator::{map, map_res, opt},
    error::ErrorKind,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::net::Ipv4Addr;
use std::process;

#[derive(Debug, PartialEq)]
struct NtpTimestamp {
    seconds: u32,
    fraction: u32,
}

#[derive(Debug, PartialEq)]
struct NtpPacket {
    leap: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_id: [u8; 4],
    reference_timestamp: NtpTimestamp,
    originate_timestamp: NtpTimestamp,
    receive_timestamp: NtpTimestamp,
    transmit_timestamp: NtpTimestamp,
}

fn ntp_timestamp(input: &[u8]) -> IResult<&[u8], NtpTimestamp> {
    let (input, seconds) = be_u32(input)?;
    let (input, fraction) = be_u32(input)?;
    Ok((input, NtpTimestamp { seconds, fraction }))
}

fn ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, leap) = be_u8(input)?;
    let (input, version_mode_stratum) = be_u8(input)?;
    let (input, poll) = map_res(be_i8, |i| {
        if i.abs() > 17 {
            Err(ErrorKind::Custom(1))
        } else {
            Ok(i)
        }
    })(input)?;
    let (input, precision) = map_res(be_i8, |i| {
        if i.abs() > 17 {
            Err(ErrorKind::Custom(1))
        } else {
            Ok(i)
        }
    })(input)?;

    let leap = (leap >> 6) & 0b11;
    let version = (version_mode_stratum >> 6) & 0b111;
    let mode = (version_mode_stratum >> 3) & 0b111;
    let stratum = version_mode_stratum & 0b111;

    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_id) = take(4usize)(input)?;
    let (input, reference_timestamp) = ntp_timestamp(input)?;
    let (input, originate_timestamp) = ntp_timestamp(input)?;
    let (input, receive_timestamp) = ntp_timestamp(input)?;
    let (input, transmit_timestamp) = ntp_timestamp(input)?;

    Ok((
        input,
        NtpPacket {
            leap,
            version,
            mode,
            stratum,
            poll,
            precision,
            root_delay,
            root_dispersion,
            reference_id: reference_id.try_into().unwrap(),
            reference_timestamp,
            originate_timestamp,
            receive_timestamp,
            transmit_timestamp,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file {}: {}", filename, err);
            process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file {}: {}", filename, err);
            process::exit(1);
        }
    };


    match ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => {
            eprintln!("Error parsing NTP packet: {:?}", e);
            process::exit(1);
        }
    }
}
