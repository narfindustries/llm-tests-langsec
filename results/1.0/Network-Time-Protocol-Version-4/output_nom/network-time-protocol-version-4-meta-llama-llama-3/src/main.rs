use nom::{
    bytes::complete::{take, take_till},
    combinator::{map, map_opt, map_res},
    error::{context, ContextError, ParseError},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{delimited, pair, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug, PartialEq)]
enum LeapIndicator {
    NoWarning,
    LastMinuteHas61Seconds,
    LastMinuteHas59Seconds,
    AlarmCondition,
}

impl From<u8> for LeapIndicator {
    fn from(val: u8) -> Self {
        match val {
            0 => LeapIndicator::NoWarning,
            1 => LeapIndicator::LastMinuteHas61Seconds,
            2 => LeapIndicator::LastMinuteHas59Seconds,
            3 => LeapIndicator::AlarmCondition,
            _ => panic!("Invalid value for LeapIndicator"),
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
    fn from(val: u8) -> Self {
        match val {
            0 => Mode::Reserved,
            1 => Mode::SymmetricActive,
            2 => Mode::SymmetricPassive,
            3 => Mode::Client,
            4 => Mode::Server,
            5 => Mode::Broadcast,
            6 => Mode::ReservedForNTPControlMessages,
            7 => Mode::ReservedForPrivateUse,
            _ => panic!("Invalid value for Mode"),
        }
    }
}

#[derive(Debug, PartialEq)]
struct NTPHeader {
    li: LeapIndicator,
    vn: u8,
    mode: Mode,
    poll: u8,
    precision: u8,
    root_delay: u32,
    root_dispersion: u32,
    ref_id: u32,
    ref_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn parse_leap_indicator(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1u8), |val| val[0] >> 6)(input)
}

fn parse_version_number(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1u8), |val| (val[0] >> 3) & 0x7)(input)
}

fn parse_mode(input: &[u8]) -> IResult<&[u8], u8> {
    map(take(1u8), |val| val[0] & 0x7)(input)
}

fn parse_ntp_header(input: &[u8]) -> IResult<&[u8], NTPHeader> {
    context(
        "NTP Header",
        tuple((
            parse_leap_indicator,
            parse_version_number,
            parse_mode,
            be_u8,
            be_u8,
            be_u32,
            be_u32,
            be_u32,
            be_u64,
            be_u64,
            be_u64,
            be_u64,
        )),
    )(input)
    .map(|(input, (li, vn, mode, poll, precision, root_delay, root_dispersion, ref_id, ref_timestamp, origin_timestamp, receive_timestamp, transmit_timestamp))| {
        let ntp_header = NTPHeader {
            li: LeapIndicator::from(li),
            vn,
            mode: Mode::from(mode),
            poll,
            precision,
            root_delay,
            root_dispersion,
            ref_id,
            ref_timestamp,
            origin_timestamp,
            receive_timestamp,
            transmit_timestamp,
        };
        (input, ntp_header)
    })
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let path = Path::new(input_file);
    let file = File::open(path).expect("Failed to open input file");
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).expect("Failed to read input file");
    match parse_ntp_header(&input) {
        Ok((_, ntp_header)) => println!("{:?}", ntp_header),
        Err(err) => eprintln!("Error parsing input file: {:?}", err),
    }
}