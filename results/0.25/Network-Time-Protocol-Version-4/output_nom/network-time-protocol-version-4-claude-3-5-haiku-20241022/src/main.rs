use nom::{
    bits::complete::take as take_bits,
    bytes::complete::take,
    multi::many0,
    number::complete::{be_f64, be_u8, be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NtpPacket {
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
    extensions: Vec<NtpExtension>,
}

#[derive(Debug)]
struct NtpExtension {
    field_type: u16,
    length: u16,
    data: Vec<u8>,
}

fn parse_ntp_header(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, first_byte) = take_bits(8u8)(input)?;
    let leap_indicator = (first_byte >> 6) & 0b11;
    let version = (first_byte >> 3) & 0b111;
    let mode = first_byte & 0b111;

    let (input, (stratum, poll_interval, precision)) = tuple((
        be_u8,
        be_u8,
        be_u8,
    ))(input)?;

    let (input, (root_delay, root_dispersion, reference_id)) = tuple((
        be_f64,
        be_f64,
        be_u32,
    ))(input)?;

    let (input, (
        reference_timestamp,
        originate_timestamp,
        receive_timestamp,
        transmit_timestamp,
    )) = tuple((
        be_f64,
        be_f64,
        be_f64,
        be_f64,
    ))(input)?;

    let (input, extensions) = many0(parse_extension)(input)?;

    Ok((input, NtpPacket {
        leap_indicator,
        version,
        mode,
        stratum,
        poll_interval: poll_interval as i8,
        precision: precision as i8,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        originate_timestamp,
        receive_timestamp,
        transmit_timestamp,
        extensions,
    }))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], NtpExtension> {
    let (input, field_type) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length as usize)(input)?;

    Ok((input, NtpExtension {
        field_type,
        length,
        data: data.to_vec(),
    }))
}

fn main() -> std::io::Result<()> {
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
            println!("NTP Packet: {:?}", packet);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}