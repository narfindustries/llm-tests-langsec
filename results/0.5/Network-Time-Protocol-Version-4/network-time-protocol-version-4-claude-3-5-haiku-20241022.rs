use nom::{
    bits::streaming::take as take_bits,
    bytes::complete::take,
    multi::many0,
    number::complete::{be_f64, be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
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
    field_length: u16,
    data: Vec<u8>,
}

fn parse_ntp_header(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, first_byte) = be_u8(input)?;
    let leap_indicator = (first_byte & 0b11000000) >> 6;
    let version = (first_byte & 0b00111000) >> 3;
    let mode = first_byte & 0b00000111;

    let (input, (
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        originate_timestamp,
        receive_timestamp,
        transmit_timestamp,
    )) = tuple((
        be_u8,
        be_u8,
        be_u8,
        be_f64,
        be_f64,
        be_u32,
        be_f64,
        be_f64,
        be_f64,
        be_f64,
    ))(input)?;

    let (input, extensions) = parse_extensions(input)?;

    Ok((input, NtpPacket {
        leap_indicator,
        version,
        mode,
        stratum,
        poll: poll as i8,
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

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<NtpExtension>> {
    many0(parse_extension)(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], NtpExtension> {
    let (input, field_type) = be_u16(input)?;
    let (input, field_length) = be_u16(input)?;
    let (input, data) = take(field_length)(input)?;

    Ok((input, NtpExtension {
        field_type,
        field_length,
        data: data.to_vec(),
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_file>", args[0]);
        std::process::exit(1);
    }

    let file_contents = fs::read(&args[1])?;
    match parse_ntp_header(&file_contents) {
        Ok((_, packet)) => {
            println!("NTP Packet: {:?}", packet);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}