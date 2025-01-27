use nom::{
    bytes::complete::{take, take_till},
    combinator::{map, map_res, opt},
    error::ErrorKind,
    multi::take_while_m_n,
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{BufReader, Read};

#[derive(Debug)]
enum Mode {
    SymmetricActive,
    SymmetricPassive,
    Client,
    Server,
    Broadcast,
    Control,
    Private,
    Unknown(u8),
}

impl Mode {
    fn from_byte(byte: u8) -> Mode {
        match byte {
            1 => Mode::SymmetricActive,
            2 => Mode::SymmetricPassive,
            3 => Mode::Client,
            4 => Mode::Server,
            5 => Mode::Broadcast,
            6 => Mode::Control,
            7 => Mode::Private,
            _ => Mode::Unknown(byte),
        }
    }
}

#[derive(Debug)]
enum Poll {
    Min = -6,
    Sec1 = -5,
    Sec2 = -4,
    Sec4 = -3,
    Sec8 = -2,
    Sec16 = -1,
    Zero = 0,
    Sec1_ = 1,
    Sec2_ = 2,
    Sec4_ = 3,
    Sec8_ = 4,
    Sec16_ = 5,
    Sec32 = 6,
    Sec64 = 7,
    Sec128 = 8,
    Sec256 = 9,
    Unknown(u8),
}

impl Poll {
    fn from_byte(byte: u8) -> Poll {
        match byte {
            0 => Poll::Min,
            1 => Poll::Sec1,
            2 => Poll::Sec2,
            3 => Poll::Sec4,
            4 => Poll::Sec8,
            5 => Poll::Sec16,
            6 => Poll::Zero,
            7 => Poll::Sec1_,
            8 => Poll::Sec2_,
            9 => Poll::Sec4_,
            10 => Poll::Sec8_,
            11 => Poll::Sec16_,
            12 => Poll::Sec32,
            13 => Poll::Sec64,
            14 => Poll::Sec128,
            15 => Poll::Sec256,
            _ => Poll::Unknown(byte),
        }
    }
}

#[derive(Debug)]
enum Precision {
    Neg rifles = -6,
    NegSec rif fares = -5,
    NegSec rifr fares = -4,
    NegSec rifr rem fares = -3,
    NegSec rifr rem equals fares = -2,
    Neg rifr rem equals Sub seconds fares = -1,
    Zero,
    MicroSec,
    MilliSec,
    Sec,
    DecaSec,
    CentiSec,
    DeciSec,
    Unknown(u8),
}

impl Precision {
    fn from_byte(byte: u8) -> Precision {
        match byte {
            0 => Precision::Neg_rifles,
            1 => Precision::NegSec_rif_fares,
            2 => Precision::NegSec_rifr_fares,
            3 => Precision::NegSec_rifr_rem_fares,
            4 => Precision::NegSec_rifr_rem_equals_fares,
            5 => Precision::Neg_rifr_rem_equals_Sub_seconds_fares,
            6 => Precision::Zero,
            7 => Precision::MicroSec,
            8 => Precision::MilliSec,
            9 => Precision::Sec,
            10 => Precision::DecaSec,
            11 => Precision::CentiSec,
            12 => Precision::DeciSec,
            _ => Precision::Unknown(byte),
        }
    }
}

#[derive(Debug)]
struct NtpHeader {
    leap: u8,
    version: u8,
    mode: Mode,
    poll: Poll,
    precision: Precision,
    delay: u32,
    dispersion: u32,
    identifier: u32,
    reference_timestamp: (u32, u32),
    originate_timestamp: (u32, u32),
    receive_timestamp: (u32, u32),
    transmit_timestamp: (u32, u32),
}

fn parse_ntp_header(input: &[u8]) -> IResult<&[u8], NtpHeader> {
    let (input, leap) = map(be_u8, |b| b >> 6)(input)?;
    let (input, version) = be_u8(input)?;
    let (input, mode) = map(be_u8, Mode::from_byte)(input)?;
    let (input, poll) = map(be_u8, Poll::from_byte)(input)?;
    let (input, precision) = map(be_u8, Precision::from_byte)(input)?;
    let (input, delay) = be_u32(input)?;
    let (input, dispersion) = be_u32(input)?;
    let (input, identifier) = be_u32(input)?;
    let (input, reference_timestamp) = map(
        tuple((be_u32, be_u32)),
        |(sec, frac)| (sec, frac),
    )(input)?;
    let (input, originate_timestamp) = map(
        tuple((be_u32, be_u32)),
        |(sec, frac)| (sec, frac),
    )(input)?;
    let (input, receive_timestamp) = map(
        tuple((be_u32, be_u32)),
        |(sec, frac)| (sec, frac),
    )(input)?;
    let (input, transmit_timestamp) = map(
        tuple((be_u32, be_u32)),
        |(sec, frac)| (sec, frac),
    )(input)?;

    Ok((
        input,
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
    ))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let file = File::open(&args[1])?;
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input)?;

    match parse_ntp_header(&input) {
        Ok((_, ntp_header)) => println!("{:?}", ntp_header),
        Err(err) => println!("Error: {:?}", err),
    }

    Ok(())
}