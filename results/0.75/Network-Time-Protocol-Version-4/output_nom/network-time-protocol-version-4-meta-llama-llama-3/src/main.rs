use nom::{
    bytes::complete::{take},
    combinator::{map},
    number::complete::{be_u8, be_u32},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug)]
enum LeapIndicator {
    NoWarning,
    LastMinute61Seconds,
    LastMinute59Seconds,
    AlarmCondition,
}

#[derive(Debug)]
enum Mode {
    Reserved,
    SymmetricActive,
    SymmetricPassive,
    Client,
    Server,
    Broadcast,
    ReservedForNtpControlMessages,
    ReservedForPrivateUse,
}

#[derive(Debug)]
struct NtpHeader {
    leap_indicator: LeapIndicator,
    version_number: u8,
    mode: Mode,
    stratum: u8,
    poll_interval: u8,
    precision: i8,
    root_delay: i32,
    root_dispersion: u32,
    reference_clock_identifier: [u8; 4],
    reference_timestamp: (u32, u32),
    origin_timestamp: (u32, u32),
    receive_timestamp: (u32, u32),
    transmit_timestamp: (u32, u32),
}

impl NtpHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, first_byte) = map(be_u8, |x| x)(input)?;

        let leap_indicator = match first_byte >> 6 {
            0 => LeapIndicator::NoWarning,
            1 => LeapIndicator::LastMinute61Seconds,
            2 => LeapIndicator::LastMinute59Seconds,
            3 => LeapIndicator::AlarmCondition,
            _ => panic!("Invalid leap indicator"),
        };

        let version_number = (first_byte >> 3) & 0x07;

        let mode = match first_byte & 0x07 {
            0 => Mode::Reserved,
            1 => Mode::SymmetricActive,
            2 => Mode::SymmetricPassive,
            3 => Mode::Client,
            4 => Mode::Server,
            5 => Mode::Broadcast,
            6 => Mode::ReservedForNtpControlMessages,
            7 => Mode::ReservedForPrivateUse,
            _ => panic!("Invalid mode"),
        };

        let (input, stratum) = be_u8(input)?;

        let (input, poll_interval) = be_u8(input)?;

        let (input, precision) = be_u8(input)?;

        let (input, root_delay) = map(be_u32, |x| x as i32)(input)?;

        let (input, root_dispersion) = be_u32(input)?;

        let (input, reference_clock_identifier) = take(4u8)(input)?;

        let (input, reference_timestamp) = map(
            tuple((be_u32, be_u32)),
            |(high, low)| (high, low),
        )(input)?;

        let (input, origin_timestamp) = map(
            tuple((be_u32, be_u32)),
            |(high, low)| (high, low),
        )(input)?;

        let (input, receive_timestamp) = map(
            tuple((be_u32, be_u32)),
            |(high, low)| (high, low),
        )(input)?;

        let (input, transmit_timestamp) = map(
            tuple((be_u32, be_u32)),
            |(high, low)| (high, low),
        )(input)?;

        Ok((
            input,
            NtpHeader {
                leap_indicator,
                version_number,
                mode,
                stratum,
                poll_interval,
                precision: precision as i8,
                root_delay,
                root_dispersion,
                reference_clock_identifier: {
                    let mut arr = [0u8; 4];
                    arr.copy_from_slice(reference_clock_identifier);
                    arr
                },
                reference_timestamp,
                origin_timestamp,
                receive_timestamp,
                transmit_timestamp,
            },
        ))
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return Ok(());
    }

    let path = Path::new(&args[1]);
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer)?;

    match NtpHeader::parse(&buffer) {
        Ok((_, header)) => println!("{:?}", header),
        Err(err) => eprintln!("Error parsing NTP header: {:?}", err),
    }

    Ok(())
}