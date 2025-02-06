use nom::{
    bits::complete::{take as bits_take, tag},
    bytes::complete::take,
    combinator::map,
    error::{Error, ErrorKind},
    number::complete::{be_i32, be_i8, be_u16, be_u32, be_u64, u8 as be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub struct NTPPacket {
    li: u8,
    vn: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: i32,
    root_dispersion: u32,
    reference_id: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extensions: Vec<ExtensionField>,
    mac: Option<MAC>,
}

#[derive(Debug)]
pub struct ExtensionField {
    field_type: u16,
    length: u16,
    value: Vec<u8>,
}

#[derive(Debug)]
pub struct MAC {
    key_id: u32,
    digest: Vec<u8>,
}

fn parse_first_byte(input: &[u8]) -> IResult<&[u8], (u8, u8, u8)> {
    nom::bits::bits(tuple((
        bits_take::<_, u8, usize, Error<(&[u8], usize)>>(2usize),
        bits_take::<_, u8, usize, Error<(&[u8], usize)>>(3usize),
        bits_take::<_, u8, usize, Error<(&[u8], usize)>>(3usize),
    )))(input)
}

fn parse_extension_field(input: &[u8]) -> IResult<&[u8], ExtensionField> {
    let (input, field_type) = be_u16(input)?;
    let (input, length) = be_u16(input)?;
    let (input, value) = take(length as usize)(input)?;
    
    Ok((input, ExtensionField {
        field_type,
        length,
        value: value.to_vec(),
    }))
}

fn parse_mac(input: &[u8]) -> IResult<&[u8], MAC> {
    let (input, key_id) = be_u32(input)?;
    let (input, digest) = take(20usize)(input)?;
    
    Ok((input, MAC {
        key_id,
        digest: digest.to_vec(),
    }))
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    let (input, (li, vn, mode)) = parse_first_byte(input)?;
    let (input, (
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp
    )) = tuple((
        be_u8,
        be_i8,
        be_i8,
        be_i32,
        be_u32,
        be_u32,
        be_u64,
        be_u64,
        be_u64,
        be_u64,
    ))(input)?;

    let mut extensions = Vec::new();
    let mut remaining = input;
    let mut mac = None;

    while remaining.len() >= 4 {
        match parse_extension_field(remaining) {
            Ok((new_remaining, extension)) => {
                extensions.push(extension);
                remaining = new_remaining;
            }
            Err(_) => break,
        }
    }

    if remaining.len() >= 24 {
        if let Ok((new_remaining, parsed_mac)) = parse_mac(remaining) {
            mac = Some(parsed_mac);
            remaining = new_remaining;
        }
    }

    Ok((remaining, NTPPacket {
        li,
        vn,
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
        mac,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_packet_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed NTP packet: {:#?}", packet);
            println!("Remaining unparsed bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}