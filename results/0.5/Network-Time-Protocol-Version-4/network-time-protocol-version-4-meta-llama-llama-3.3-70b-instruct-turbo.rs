use nom::{
    bytes::complete::{take},
    combinator::{map},
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    IResult,
};
use std::{env, fs};

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
    ReservedForNTPControlMessages,
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
            6 => Mode::ReservedForNTPControlMessages,
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
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_clock_identifier: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn parse_leap_indicator(input: &[u8]) -> IResult<&[u8], LeapIndicator> {
    map(be_u8, |x| LeapIndicator::from(x >> 6 & 0x3))(input)
}

fn parse_version_number(input: &[u8]) -> IResult<&[u8], u8> {
    map(be_u8, |x| x >> 3 & 0x7)(input)
}

fn parse_mode(input: &[u8]) -> IResult<&[u8], Mode> {
    map(be_u8, |x| Mode::from(x & 0x7))(input)
}

fn parse_poll(input: &[u8]) -> IResult<&[u8], u8> {
    be_u8(input)
}

fn parse_precision(input: &[u8]) -> IResult<&[u8], i8> {
    map(be_u8, |x| x as i8)(input)
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
    let (input, leap_indicator) = parse_leap_indicator(input)?;
    let (input, version_number) = parse_version_number(input)?;
    let (input, mode) = parse_mode(input)?;
    let (input, poll) = parse_poll(input)?;
    let (input, precision) = parse_precision(input)?;
    let (input, root_delay) = parse_root_delay(input)?;
    let (input, root_dispersion) = parse_root_dispersion(input)?;
    let (input, reference_clock_identifier) = parse_reference_clock_identifier(input)?;
    let (input, reference_timestamp) = parse_reference_timestamp(input)?;
    let (input, origin_timestamp) = parse_origin_timestamp(input)?;
    let (input, receive_timestamp) = parse_receive_timestamp(input)?;
    let (input, transmit_timestamp) = parse_transmit_timestamp(input)?;

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

#[derive(Debug, PartialEq)]
struct ExtensionField {
    field_type: u16,
    field_length: u16,
    field_data: Vec<u8>,
}

fn parse_extension_field(input: &[u8]) -> IResult<&[u8], ExtensionField> {
    let (input, field_type) = be_u16(input)?;
    let (input, field_length) = be_u16(input)?;
    let (input, field_data) = take(field_length as usize)(input)?;

    Ok((
        input,
        ExtensionField {
            field_type,
            field_length,
            field_data: field_data.to_vec(),
        },
    ))
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], (NTPHeader, Vec<ExtensionField>)> {
    let (input, header) = parse_ntp_header(input)?;
    let mut extensions = Vec::new();
    let mut remaining = input;
    while remaining.len() >= 4 {
        let (rest, extension) = parse_extension_field(remaining)?;
        extensions.push(extension);
        remaining = rest;
    }

    Ok((remaining, (header, extensions)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let input_file = &args[1];
    let input_data = match fs::read(input_file) {
        Ok(data) => data,
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match parse_ntp_packet(&input_data) {
        Ok((remaining, (header, extensions))) => {
            println!("Header: {:?}", header);
            println!("Extensions: {:?}", extensions);
            if !remaining.is_empty() {
                println!("Warning: remaining data after parsing packet");
            }
        }
        Err(err) => {
            println!("Error parsing packet: {:?}", err);
        }
    }
}