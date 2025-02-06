use nom::{
    bits::complete::{tag, take},
    combinator::map,
    error::Error,
    sequence::tuple,
    IResult,
};
use std::{env, fs, path::Path};

#[derive(Debug)]
pub struct NTPPacket {
    li: u8,
    vn: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
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

fn parse_first_byte(input: (&[u8], usize)) -> IResult<(&[u8], usize), (u8, u8, u8)> {
    tuple((
        take(2usize),  // LI
        take(3usize),  // VN
        take(3usize),  // Mode
    ))(input)
}

fn parse_fixed_point_16_16(input: &[u8]) -> f32 {
    let integer = ((input[0] as u32) << 8 | input[1] as u32) as i32;
    let fraction = ((input[2] as u32) << 8 | input[3] as u32) as f32 / 65536.0;
    integer as f32 + fraction
}

fn parse_timestamp(input: &[u8]) -> u64 {
    let seconds = ((input[0] as u64) << 24)
        | ((input[1] as u64) << 16)
        | ((input[2] as u64) << 8)
        | (input[3] as u64);
    let fraction = ((input[4] as u64) << 24)
        | ((input[5] as u64) << 16)
        | ((input[6] as u64) << 8)
        | (input[7] as u64);
    (seconds << 32) | fraction
}

fn parse_extension_field(input: &[u8]) -> Option<(ExtensionField, &[u8])> {
    if input.len() < 4 {
        return None;
    }
    
    let field_type = ((input[0] as u16) << 8) | (input[1] as u16);
    let length = ((input[2] as u16) << 8) | (input[3] as u16);
    
    if input.len() < length as usize {
        return None;
    }

    let value = input[4..length as usize].to_vec();
    let padding = (4 - (length % 4)) % 4;
    let next_input = &input[(length + padding) as usize..];

    Some((ExtensionField {
        field_type,
        length,
        value,
    }, next_input))
}

fn parse_mac(input: &[u8]) -> Option<MAC> {
    if input.len() < 24 {
        return None;
    }

    let key_id = ((input[0] as u32) << 24)
        | ((input[1] as u32) << 16)
        | ((input[2] as u32) << 8)
        | (input[3] as u32);
    
    let digest = input[4..24].to_vec();

    Some(MAC { key_id, digest })
}

fn parse_ntp_packet(input: &[u8]) -> Option<NTPPacket> {
    if input.len() < 48 {
        return None;
    }

    let (_, (li, vn, mode)) = parse_first_byte((&input[0..1], 0)).ok()?;
    
    let mut packet = NTPPacket {
        li,
        vn,
        mode,
        stratum: input[1],
        poll: input[2] as i8,
        precision: input[3] as i8,
        root_delay: parse_fixed_point_16_16(&input[4..8]),
        root_dispersion: parse_fixed_point_16_16(&input[8..12]),
        reference_id: ((input[12] as u32) << 24)
            | ((input[13] as u32) << 16)
            | ((input[14] as u32) << 8)
            | (input[15] as u32),
        reference_timestamp: parse_timestamp(&input[16..24]),
        origin_timestamp: parse_timestamp(&input[24..32]),
        receive_timestamp: parse_timestamp(&input[32..40]),
        transmit_timestamp: parse_timestamp(&input[40..48]),
        extensions: Vec::new(),
        mac: None,
    };

    let mut remaining = &input[48..];
    while remaining.len() >= 4 {
        if let Some((extension, next_input)) = parse_extension_field(remaining) {
            packet.extensions.push(extension);
            remaining = next_input;
        } else {
            break;
        }
    }

    if !remaining.is_empty() {
        packet.mac = parse_mac(remaining);
    }

    Some(packet)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_packet_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let data = match fs::read(path) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            std::process::exit(1);
        }
    };

    match parse_ntp_packet(&data) {
        Some(packet) => println!("{:#?}", packet),
        None => eprintln!("Failed to parse NTP packet"),
    }
}