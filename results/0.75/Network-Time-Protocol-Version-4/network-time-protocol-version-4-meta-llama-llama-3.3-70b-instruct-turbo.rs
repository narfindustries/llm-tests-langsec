use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{cond, map, map_opt, opt},
    error::{context, ErrorKind},
    multi::{length_data, many_till, separator},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufRead, BufReader, Read},
};
use std::convert::TryInto;

#[derive(Debug, PartialEq)]
enum LeapIndicator {
    NoWarning,
    LastMinuteHas61Seconds,
    LastMinuteHas59Seconds,
    Unknown,
}

impl LeapIndicator {
    fn from_u8(value: u8) -> LeapIndicator {
        match value {
            0 => LeapIndicator::NoWarning,
            1 => LeapIndicator::LastMinuteHas61Seconds,
            2 => LeapIndicator::LastMinuteHas59Seconds,
            3 => LeapIndicator::Unknown,
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
    Reserved2,
    Reserved3,
    Reserved4,
}

impl Mode {
    fn from_u8(value: u8) -> Mode {
        match value {
            0 => Mode::Reserved,
            1 => Mode::SymmetricActive,
            2 => Mode::SymmetricPassive,
            3 => Mode::Client,
            4 => Mode::Server,
            5 => Mode::Broadcast,
            6 => Mode::Reserved2,
            7 => Mode::Reserved3,
            _ => Mode::Reserved4,
        }
    }
}

#[derive(Debug, PartialEq)]
struct NtpHeader {
    leap: LeapIndicator,
    version: u8,
    mode: Mode,
    poll: u8,
    precision: u8,
    delay: u32,
    dispersion: u32,
    identifier: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

impl NtpHeader {
    fn parse(i: &[u8]) -> IResult<&[u8], NtpHeader> {
        context(
            "ntp header",
            tuple((
                map_opt(be_u8, |value| {
                    let leap = (value >> 6) & 0x3;
                    Some(LeapIndicator::from_u8(leap))
                }),
                map_opt(be_u8, |value| {
                    let version = (value >> 3) & 0x7;
                    Some(version)
                }),
                map_opt(be_u8, |value| {
                    let mode = value & 0x7;
                    Some(Mode::from_u8(mode))
                }),
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
        )(i)
        .map(|(i, (leap, version, mode, poll, precision, delay, dispersion, identifier, reference_timestamp, originate_timestamp, receive_timestamp, transmit_timestamp))| {
            (
                i,
                NtpHeader {
                    leap,
                    version,
                    mode,
                    poll,
                    precision,
                    delay,
                    dispersion,
                    identifier,
                    reference_timestamp,
                    originate_timestamp,
                    receive_timestamp,
                    transmit_timestamp,
                },
            )
        })
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let file = File::open(&args[1])?;
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer)?;
    let (_remaining, ntp_header) = NtpHeader::parse(&buffer).unwrap();
    println!("{:?}", ntp_header);
    Ok(())
}