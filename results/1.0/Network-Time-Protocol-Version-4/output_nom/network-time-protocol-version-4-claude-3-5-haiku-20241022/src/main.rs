use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    bytes::streaming::{tag, take},
    combinator::{map, opt},
    error::Error,
    multi::many0,
    number::streaming::{be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult, Parser,
};

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: u32,
    reference_timestamp: f64,
    origin_timestamp: f64,
    receive_timestamp: f64,
    transmit_timestamp: f64,
    extensions: Option<Vec<NtpExtension>>,
    authentication_data: Option<NtpAuthentication>,
}

#[derive(Debug)]
struct NtpExtension {
    field_type: u16,
    length: u16,
    data: Vec<u8>,
}

#[derive(Debug)]
struct NtpAuthentication {
    key_id: u32,
    message_digest: Vec<u8>,
}

fn parse_ntp_header(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, (header_byte, stratum, poll, precision)) = tuple((
        take_bits(8u8),
        take(1u8),
        take(1i8),
        take(1i8),
    ))(input)?;

    let leap_indicator = (header_byte >> 6) & 0b11;
    let version = (header_byte >> 3) & 0b111;
    let mode = header_byte & 0b111;

    let (input, (root_delay, root_dispersion, reference_id)) = tuple((
        map(be_u32, |val| f32::from_bits(val)),
        map(be_u32, |val| f32::from_bits(val)),
        be_u32,
    ))(input)?;

    let (input, (reference_timestamp, origin_timestamp, receive_timestamp, transmit_timestamp)) = tuple((
        map(be_u64, |val| f64::from_bits(val)),
        map(be_u64, |val| f64::from_bits(val)),
        map(be_u64, |val| f64::from_bits(val)),
        map(be_u64, |val| f64::from_bits(val)),
    ))(input)?;

    let (input, extensions) = opt(parse_extensions)(input)?;
    let (input, authentication) = opt(parse_authentication)(input)?;

    Ok((input, NtpPacket {
        leap_indicator,
        version,
        mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp,
        extensions,
        authentication_data: authentication,
    }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<NtpExtension>> {
    many0(|i| {
        let (i, field_type) = be_u16(i)?;
        let (i, length) = be_u16(i)?;
        let (i, data) = take(length)(i)?;
        Ok((i, NtpExtension { field_type, length, data: data.to_vec() }))
    })(input)
}

fn parse_authentication(input: &[u8]) -> IResult<&[u8], NtpAuthentication> {
    let (input, key_id) = be_u32(input)?;
    let (input, message_digest) = take(16usize)(input)?;
    
    Ok((input, NtpAuthentication {
        key_id,
        message_digest: message_digest.to_vec(),
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

    match parse_ntp_header(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed NTP Packet: {:?}", packet);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {:?}", e);
            Err(Box::new(Error::from(e)))
        }
    }
}