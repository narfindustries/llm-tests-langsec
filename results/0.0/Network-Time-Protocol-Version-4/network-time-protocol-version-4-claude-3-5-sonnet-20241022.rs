use nom::{
    bits::complete::take,
    error::Error,
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NtpPacket {
    li: u8,
    vn: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: [u8; 4],
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extensions: Vec<ExtensionField>,
    mac: Option<Mac>,
}

#[derive(Debug)]
struct ExtensionField {
    field_type: u16,
    length: u16,
    value: Vec<u8>,
}

#[derive(Debug)]
struct Mac {
    key_id: u32,
    digest: Vec<u8>,
}

fn parse_first_byte(input: &[u8]) -> IResult<&[u8], (u8, u8, u8)> {
    nom::bits::bits(tuple((
        take::<_, u8, usize, Error<(&[u8], usize)>>(2usize),
        take::<_, u8, usize, Error<(&[u8], usize)>>(3usize),
        take::<_, u8, usize, Error<(&[u8], usize)>>(3usize),
    )))(input)
}

fn parse_fixed_point_16_16(input: &[u8]) -> IResult<&[u8], f32> {
    let (input, bytes) = nom::number::complete::be_u32(input)?;
    let integer = ((bytes >> 16) & 0xFFFF) as i16;
    let fraction = (bytes & 0xFFFF) as u16;
    let value = integer as f32 + (fraction as f32 / 65536.0);
    Ok((input, value))
}

fn parse_timestamp(input: &[u8]) -> IResult<&[u8], u64> {
    nom::number::complete::be_u64(input)
}

fn parse_extension_field(input: &[u8]) -> IResult<&[u8], ExtensionField> {
    let (input, field_type) = nom::number::complete::be_u16(input)?;
    let (input, length) = nom::number::complete::be_u16(input)?;
    let (input, value) = nom::bytes::complete::take(length as usize)(input)?;
    
    Ok((input, ExtensionField {
        field_type,
        length,
        value: value.to_vec(),
    }))
}

fn parse_mac(input: &[u8]) -> IResult<&[u8], Mac> {
    let (input, key_id) = nom::number::complete::be_u32(input)?;
    let (input, digest) = nom::bytes::complete::take(20usize)(input)?;
    
    Ok((input, Mac {
        key_id,
        digest: digest.to_vec(),
    }))
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, (li, vn, mode)) = parse_first_byte(input)?;
    let (input, stratum) = nom::number::complete::u8(input)?;
    let (input, poll) = nom::number::complete::i8(input)?;
    let (input, precision) = nom::number::complete::i8(input)?;
    let (input, root_delay) = parse_fixed_point_16_16(input)?;
    let (input, root_dispersion) = parse_fixed_point_16_16(input)?;
    let (input, reference_id) = nom::bytes::complete::take(4usize)(input)?;
    let (input, reference_timestamp) = parse_timestamp(input)?;
    let (input, origin_timestamp) = parse_timestamp(input)?;
    let (input, receive_timestamp) = parse_timestamp(input)?;
    let (input, transmit_timestamp) = parse_timestamp(input)?;

    let mut remaining = input;
    let mut extensions = Vec::new();
    let mut mac = None;

    while remaining.len() >= 4 {
        if let Ok((new_remaining, extension)) = parse_extension_field(remaining) {
            extensions.push(extension);
            remaining = new_remaining;
        } else {
            break;
        }
    }

    if remaining.len() >= 24 {
        if let Ok((new_remaining, parsed_mac)) = parse_mac(remaining) {
            mac = Some(parsed_mac);
            remaining = new_remaining;
        }
    }

    Ok((remaining, NtpPacket {
        li,
        vn,
        mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id: reference_id.try_into().unwrap(),
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