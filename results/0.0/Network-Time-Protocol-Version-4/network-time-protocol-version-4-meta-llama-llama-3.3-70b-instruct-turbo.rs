use nom::{
    bytes::complete::{take},
    combinator::{map},
    error::{Error, ErrorKind},
    number::complete::{be_u8, be_u32, be_u64},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
enum LeapIndicator {
    NoWarning,
    LastMinuteHas61Seconds,
    LastMinuteHas59Seconds,
    AlarmCondition,
}

impl From<u8> for LeapIndicator {
    fn from(value: u8) -> Self {
        match value {
            0 => LeapIndicator::NoWarning,
            1 => LeapIndicator::LastMinuteHas61Seconds,
            2 => LeapIndicator::LastMinuteHas59Seconds,
            3 => LeapIndicator::AlarmCondition,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Mode {
    Reserved,
    SymmetricActive,
    SymmetricPassive,
    Client,
    Server,
    Broadcast,
    ReservedForNTPControlMessage,
    ReservedForPrivateUse,
}

impl From<u8> for Mode {
    fn from(value: u8) -> Self {
        match value {
            0 => Mode::Reserved,
            1 => Mode::SymmetricActive,
            2 => Mode::SymmetricPassive,
            3 => Mode::Client,
            4 => Mode::Server,
            5 => Mode::Broadcast,
            6 => Mode::ReservedForNTPControlMessage,
            7 => Mode::ReservedForPrivateUse,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq)]
struct NTPHeader {
    leap_indicator: LeapIndicator,
    version_number: u8,
    mode: Mode,
    poll: u8,
    precision: u8,
    root_delay: u32,
    root_dispersion: u32,
    reference_clock_identifier: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn parse_leap_indicator(input: &[u8]) -> IResult<&[u8], LeapIndicator> {
    map(be_u8, |value| LeapIndicator::from(value >> 6))(input)
}

fn parse_version_number(input: &[u8]) -> IResult<&[u8], u8> {
    map(be_u8, |value| value & 0x1f)(input)
}

fn parse_mode(input: &[u8]) -> IResult<&[u8], Mode> {
    map(be_u8, |value| Mode::from(value & 0x07))(input)
}

fn parse_poll(input: &[u8]) -> IResult<&[u8], u8> {
    be_u8(input)
}

fn parse_precision(input: &[u8]) -> IResult<&[u8], u8> {
    be_u8(input)
}

fn parse_root_delay(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn parse_root_dispersion(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn parse_reference_clock_identifier(input: &[u8]) -> IResult<&[u8], u32> {
    be_u32(input)
}

fn parse_reference_timestamp(input: &[u8]) -> IResult<&[u8], u64> {
    be_u64(input)
}

fn parse_origin_timestamp(input: &[u8]) -> IResult<&[u8], u64> {
    be_u64(input)
}

fn parse_receive_timestamp(input: &[u8]) -> IResult<&[u8], u64> {
    be_u64(input)
}

fn parse_transmit_timestamp(input: &[u8]) -> IResult<&[u8], u64> {
    be_u64(input)
}

fn parse_ntp_header(input: &[u8]) -> IResult<&[u8], NTPHeader> {
    let (input, first_byte) = be_u8(input)?;
    let leap_indicator = LeapIndicator::from(first_byte >> 6);
    let version_number = first_byte & 0x1f;
    let mode = Mode::from(first_byte & 0x07);
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_clock_identifier) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, origin_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    Ok((
        input,
        NTPHeader {
            leap_indicator,
            version_number,
            mode,
            poll,
            precision,
            root_delay,
            root_dispersion,
            reference_clock_identifier,
            reference_timestamp,
            origin_timestamp,
            receive_timestamp,
            transmit_timestamp,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let file = File::open(&args[1]).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).expect("Failed to read file");

    match parse_ntp_header(&input) {
        Ok((remaining, header)) => {
            println!("Parsed NTP header: {:?}", header);
            if !remaining.is_empty() {
                println!("Remaining input: {:?}", remaining);
            }
        }
        Err(err) => {
            eprintln!("Error parsing NTP header: {:?}", err);
        }
    }
}