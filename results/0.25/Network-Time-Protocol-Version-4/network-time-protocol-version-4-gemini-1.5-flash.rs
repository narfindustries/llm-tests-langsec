use nom::{
    be::{be_u16, be_u32, be_u8},
    bytes::complete::take,
    combinator::{map, map_res, opt, value},
    error::ErrorKind,
    multi::count,
    number::complete::le_u32,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::net::{IpAddr, Ipv4Addr};
use std::path::Path;

#[derive(Debug, PartialEq)]
enum LeapIndicator {
    NoWarning,
    LastMinute61,
    LastMinute59,
    AlarmingConditions,
}

#[derive(Debug, PartialEq)]
enum Mode {
    SymmetricActive,
    SymmetricPassive,
    Client,
    Server,
    Broadcast,
    Reserved,
}

#[derive(Debug, PartialEq)]
enum Stratum {
    Unspecified,
    PrimaryReference,
    SecondaryReference,
    LocalClock,
}

#[derive(Debug, PartialEq)]
struct NtpPacket {
    leap_indicator: LeapIndicator,
    version_number: u8,
    mode: Mode,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_id: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn leap_indicator(input: &[u8]) -> IResult<&[u8], LeapIndicator> {
    map(be_u8, |value| match value & 0b11 {
        0 => LeapIndicator::NoWarning,
        1 => LeapIndicator::LastMinute61,
        2 => LeapIndicator::LastMinute59,
        3 => LeapIndicator::AlarmingConditions,
        _ => unreachable!(),
    })(input)
}

fn mode(input: &[u8]) -> IResult<&[u8], Mode> {
    map(be_u8, |value| match value & 0b111 {
        1 => Mode::SymmetricActive,
        2 => Mode::SymmetricPassive,
        3 => Mode::Client,
        4 => Mode::Server,
        5 => Mode::Broadcast,
        _ => Mode::Reserved,
    })(input)
}

fn stratum(input: &[u8]) -> IResult<&[u8], Stratum> {
    map(be_u8, |value| match value {
        0 => Stratum::Unspecified,
        1 => Stratum::PrimaryReference,
        _ => Stratum::SecondaryReference,
    })(input)
}

fn ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, leap_indicator) = leap_indicator(input)?;
    let (input, version_number) = be_u8(input)?;
    let (input, mode) = mode(input)?;
    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_i8(input)?;
    let (input, precision) = be_i8(input)?;
    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    Ok((
        input,
        NtpPacket {
            leap_indicator,
            version_number,
            mode,
            stratum: stratum,
            poll,
            precision,
            root_delay,
            root_dispersion,
            reference_id,
            reference_timestamp,
            originate_timestamp,
            receive_timestamp,
            transmit_timestamp,
        },
    ))
}

fn be_i8(i: &[u8]) -> IResult<&[u8], i8> {
    map_res(be_u8, |x| x as i8)(i)
}

fn be_u64(i: &[u8]) -> IResult<&[u8], u64> {
    let (i, a) = be_u32(i)?;
    let (i, b) = be_u32(i)?;
    Ok((i, ((a as u64) << 32) | b as u64))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Failed to open file: {}", err);
            std::process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Failed to read file: {}", err);
            std::process::exit(1);
        }
    };

    match ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Error parsing NTP packet: {:?}", e),
    }
}
