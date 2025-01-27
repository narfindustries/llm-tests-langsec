use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    bytes::streaming::{tag, take},
    combinator::{map, opt},
    error::ErrorKind,
    multi::count,
    number::streaming::{be_u8, be_u16, be_u32, be_i64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NTPPacket {
    leap_indicator: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll_interval: i8,
    precision: i8,
    root_delay: f64,
    root_dispersion: f64,
    reference_id: u32,
    reference_timestamp: f64,
    originate_timestamp: f64,
    receive_timestamp: f64,
    transmit_timestamp: f64,
    extensions: Option<Vec<NTPExtension>>,
    authentication_data: Option<NTPAuthentication>,
}

#[derive(Debug)]
struct NTPExtension {
    field_type: u16,
    length: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct NTPAuthentication {
    key_id: u32,
    digest: Vec<u8>,
}

fn parse_ntp_timestamp(input: &[u8]) -> IResult<&[u8], f64> {
    map(be_u64, |val| {
        let seconds = (val >> 32) as u32;
        let fraction = (val & 0xFFFFFFFF) as u32;
        seconds as f64 + fraction as f64 / (1u64 << 32) as f64
    })(input)
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    let (input, (first_byte, stratum, poll_interval, precision)) = tuple((
        be_u8,
        be_u8,
        be_i8,
        be_i8,
    ))(input)?;

    let leap_indicator = (first_byte >> 6) & 0b11;
    let version = (first_byte >> 3) & 0b111;
    let mode = first_byte & 0b111;

    let (input, (root_delay, root_dispersion, reference_id)) = tuple((
        map(be_u16, |val| val as f64 / 65536.0),
        map(be_u16, |val| val as f64 / 65536.0),
        be_u32,
    ))(input)?;

    let (input, (reference_timestamp, originate_timestamp, receive_timestamp, transmit_timestamp)) = tuple((
        parse_ntp_timestamp,
        parse_ntp_timestamp,
        parse_ntp_timestamp,
        parse_ntp_timestamp,
    ))(input)?;

    let (input, extensions) = opt(parse_extensions)(input)?;
    let (input, authentication_data) = opt(parse_authentication)(input)?;

    Ok((input, NTPPacket {
        leap_indicator,
        version,
        mode,
        stratum,
        poll_interval,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        originate_timestamp,
        receive_timestamp,
        transmit_timestamp,
        extensions,
        authentication_data,
    }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<NTPExtension>> {
    let (input, extensions) = many0(parse_extension)(input)?;
    Ok((input, extensions))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], NTPExtension> {
    let (input, (field_type, length)) = tuple((be_u16, be_u16))(input)?;
    let (input, data) = take(length as usize)(input)?;

    Ok((input, NTPExtension {
        field_type,
        length,
        data: data.to_vec(),
    }))
}

fn parse_authentication(input: &[u8]) -> IResult<&[u8], NTPAuthentication> {
    let (input, key_id) = be_u32(input)?;
    let (input, digest) = take(16usize)(input)?;

    Ok((input, NTPAuthentication {
        key_id,
        digest: digest.to_vec(),
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed NTP Packet: {:?}", packet);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {:?}", e);
            std::process::exit(1);
        }
    }
}