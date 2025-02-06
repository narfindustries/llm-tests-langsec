use nom::{
    bits::streaming::take as take_bits,
    bytes::streaming::take,
    number::streaming::{be_u8, be_u32, be_u64},
    sequence::tuple,
    IResult,
    combinator::map,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum LeapIndicator {
    NoWarning,
    LastMinute61Seconds,
    LastMinute59Seconds,
    Alarm,
}

#[derive(Debug, PartialEq)]
enum NtpMode {
    Reserved,
    SymmetricActive,
    SymmetricPassive,
    Client,
    Server,
    Broadcast,
    ControlMessage,
    PrivateUse,
}

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: LeapIndicator,
    version: u8,
    mode: NtpMode,
    stratum: u8,
    poll_interval: i8,
    precision: i8,
    root_delay: f64,
    root_dispersion: f64,
    reference_identifier: Vec<u8>,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn parse_leap_indicator(input: (&[u8], usize)) -> IResult<(&[u8], usize), LeapIndicator> {
    map(take_bits(2usize), |val: u8| match val {
        0b00 => LeapIndicator::NoWarning,
        0b01 => LeapIndicator::LastMinute61Seconds,
        0b10 => LeapIndicator::LastMinute59Seconds,
        0b11 => LeapIndicator::Alarm,
        _ => unreachable!(),
    })(input)
}

fn parse_ntp_mode(input: (&[u8], usize)) -> IResult<(&[u8], usize), NtpMode> {
    map(take_bits(3usize), |val: u8| match val {
        0b000 => NtpMode::Reserved,
        0b001 => NtpMode::SymmetricActive,
        0b010 => NtpMode::SymmetricPassive,
        0b011 => NtpMode::Client,
        0b100 => NtpMode::Server,
        0b101 => NtpMode::Broadcast,
        0b110 => NtpMode::ControlMessage,
        0b111 => NtpMode::PrivateUse,
        _ => unreachable!(),
    })(input)
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, ((leap_indicator, version, mode),
                 stratum,
                 poll_interval,
                 precision,
                 root_delay,
                 root_dispersion,
                 reference_identifier,
                 reference_timestamp,
                 origin_timestamp,
                 receive_timestamp,
                 transmit_timestamp)) = tuple((
        tuple((
            |i| parse_leap_indicator((i, 0)),
            |i| take_bits(3usize)((i, 0)),
            |i| parse_ntp_mode((i, 0)),
        )),
        be_u8,
        be_u8,
        be_u8,
        be_u32,
        be_u32,
        take(4usize),
        be_u64,
        be_u64,
        be_u64,
        be_u64,
    ))(input)?;

    Ok((input, NtpPacket {
        leap_indicator,
        version,
        mode,
        stratum,
        poll_interval: poll_interval as i8,
        precision: precision as i8,
        root_delay: f64::from(root_delay) / 65536.0,
        root_dispersion: f64::from(root_dispersion) / 65536.0,
        reference_identifier: reference_identifier.to_vec(),
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <ntp_packet_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("NTP Packet: {:?}", packet);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}