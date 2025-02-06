use nom::{
    bits::streaming::take as take_bits,
    bytes::streaming::take,
    error::{Error, ErrorKind},
    multi::many0,
    number::streaming::{be_u8, be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
    error::ParseError,
};
use std::env;
use std::fs;
use std::convert::TryInto;

#[derive(Debug)]
enum LeapIndicator {
    NoWarning,
    ExtraSecond61,
    ExtraSecond59,
    NotSynchronized,
}

#[derive(Debug)]
enum NtpMode {
    Reserved,
    SymmetricActive,
    SymmetricPassive,
    Client,
    Server,
    Broadcast,
    ControlMessage,
    PrivateUse,
}

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: LeapIndicator,
    version: u8,
    mode: NtpMode,
    stratum: u8,
    poll_interval: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: [u8; 4],
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extension_fields: Vec<ExtensionField>,
}

#[derive(Debug)]
struct ExtensionField {
    field_type: u16,
    field_length: u16,
    data: Vec<u8>,
}

fn parse_leap_indicator(input: (&[u8], usize)) -> IResult<(&[u8], usize), LeapIndicator> {
    let (input, bits) = take_bits(2usize)(input)?;
    let leap_indicator = match bits {
        0b00 => LeapIndicator::NoWarning,
        0b01 => LeapIndicator::ExtraSecond61,
        0b10 => LeapIndicator::ExtraSecond59,
        0b11 => LeapIndicator::NotSynchronized,
        _ => return Err(nom::Err::Error(Error::from_error_kind(input, ErrorKind::Char))),
    };
    Ok((input, leap_indicator))
}

fn parse_ntp_mode(input: (&[u8], usize)) -> IResult<(&[u8], usize), NtpMode> {
    let (input, bits) = take_bits(3usize)(input)?;
    let mode = match bits {
        0b000 => NtpMode::Reserved,
        0b001 => NtpMode::SymmetricActive,
        0b010 => NtpMode::SymmetricPassive,
        0b011 => NtpMode::Client,
        0b100 => NtpMode::Server,
        0b101 => NtpMode::Broadcast,
        0b110 => NtpMode::ControlMessage,
        0b111 => NtpMode::PrivateUse,
        _ => return Err(nom::Err::Error(Error::from_error_kind(input, ErrorKind::Char))),
    };
    Ok((input, mode))
}

fn parse_extension_field(input: &[u8]) -> IResult<&[u8], ExtensionField> {
    let (input, field_type) = be_u16(input)?;
    let (input, field_length) = be_u16(input)?;
    let (input, data) = take(field_length as usize)(input)?;
    Ok((input, ExtensionField {
        field_type,
        field_length,
        data: data.to_vec(),
    }))
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, (leap_indicator, version, mode)) = tuple((
        |i| parse_leap_indicator((i, 0)),
        |i| take_bits(3usize)((i, 0)).map(|((a,_), bits)| (a, bits)),
        |i| parse_ntp_mode((i, 0)),
    ))(input)?;

    let (input, stratum) = be_u8(input)?;
    let (input, poll_interval) = be_u8(input)?;
    let (input, precision) = be_u8(input)?;

    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_id) = take(4usize)(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, origin_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let (input, extension_fields) = many0(parse_extension_field)(input)?;

    Ok((input, NtpPacket {
        leap_indicator,
        version,
        mode,
        stratum,
        poll_interval: poll_interval as i8,
        precision: precision as i8,
        root_delay: (root_delay as f64 / 65536.0) as f32,
        root_dispersion: (root_dispersion as f64 / 65536.0) as f32,
        reference_id: reference_id.try_into().unwrap(),
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp,
        extension_fields,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_packet_file>", args[0]);
        std::process::exit(1);
    }

    let file_contents = fs::read(&args[1])?;
    match parse_ntp_packet(&file_contents) {
        Ok((_, packet)) => {
            println!("NTP Packet: {:?}", packet);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}