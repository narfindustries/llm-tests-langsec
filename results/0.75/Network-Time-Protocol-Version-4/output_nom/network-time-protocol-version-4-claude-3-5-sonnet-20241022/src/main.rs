use nom::{
    bits::complete::{tag, take},
    combinator::map,
    error::Error,
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct NTPv4Packet {
    leap_indicator: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_id: [u8; 4],
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extension_fields: Vec<ExtensionField>,
    mac: Option<MAC>,
}

#[derive(Debug)]
struct ExtensionField {
    field_type: u16,
    length: u16,
    value: Vec<u8>,
}

#[derive(Debug)]
struct MAC {
    key_id: u32,
    message_digest: Vec<u8>,
}

fn parse_first_byte(input: (&[u8], usize)) -> IResult<(&[u8], usize), (u8, u8, u8)> {
    tuple((
        take(2usize),
        take(3usize),
        take(3usize),
    ))(input)
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPv4Packet> {
    let (input, ((li, vn, mode), stratum, poll, precision)) = tuple((
        parse_first_byte,
        take(8usize),
        take(8usize),
        take(8usize),
    ))((input, 0)).unwrap();

    let (input, root_delay) = nom::number::complete::be_u32(input)?;
    let (input, root_dispersion) = nom::number::complete::be_u32(input)?;
    let (input, reference_id) = nom::bytes::complete::take(4usize)(input)?;
    let (input, reference_timestamp) = nom::number::complete::be_u64(input)?;
    let (input, origin_timestamp) = nom::number::complete::be_u64(input)?;
    let (input, receive_timestamp) = nom::number::complete::be_u64(input)?;
    let (input, transmit_timestamp) = nom::number::complete::be_u64(input)?;

    let mut extension_fields = Vec::new();
    let mut remaining = input;
    
    while remaining.len() >= 4 {
        if let Ok((new_remaining, extension_field)) = parse_extension_field(remaining) {
            extension_fields.push(extension_field);
            remaining = new_remaining;
        } else {
            break;
        }
    }

    let mac = if remaining.len() >= 4 {
        match parse_mac(remaining) {
            Ok((_, mac)) => Some(mac),
            Err(_) => None,
        }
    } else {
        None
    };

    Ok((remaining, NTPv4Packet {
        leap_indicator: li,
        version: vn,
        mode,
        stratum,
        poll: poll as i8,
        precision: precision as i8,
        root_delay,
        root_dispersion,
        reference_id: reference_id.try_into().unwrap(),
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp,
        extension_fields,
        mac,
    }))
}

fn parse_extension_field(input: &[u8]) -> IResult<&[u8], ExtensionField> {
    let (input, field_type) = nom::number::complete::be_u16(input)?;
    let (input, length) = nom::number::complete::be_u16(input)?;
    let (input, value) = nom::bytes::complete::take(length as usize - 4)(input)?;

    Ok((input, ExtensionField {
        field_type,
        length,
        value: value.to_vec(),
    }))
}

fn parse_mac(input: &[u8]) -> IResult<&[u8], MAC> {
    let (input, key_id) = nom::number::complete::be_u32(input)?;
    let (input, message_digest) = nom::bytes::complete::take(input.len())(input)?;

    Ok((input, MAC {
        key_id,
        message_digest: message_digest.to_vec(),
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
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}