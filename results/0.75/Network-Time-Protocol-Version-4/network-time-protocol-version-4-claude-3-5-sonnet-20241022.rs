use nom::{
    bits::complete::take,
    combinator::verify,
    error::Error,
    multi::many0,
    number::complete::{be_i32, be_i8, be_u16, be_u32, be_u64, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NTPv4Packet {
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
struct ExtensionField {
    field_type: u16,
    length: u16,
    value: Vec<u8>,
}

#[derive(Debug)]
struct MAC {
    key_identifier: u32,
    message_digest: Vec<u8>,
}

fn parse_first_byte(input: &[u8]) -> IResult<&[u8], (u8, u8, u8)> {
    nom::bits::bits::<_, _, Error<(&[u8], usize)>, Error<(&[u8], usize)>, _>(tuple((
        verify(take(2usize), |&x: &u8| x <= 3),
        verify(take(3usize), |&x: &u8| x <= 4),
        verify(take(3usize), |&x: &u8| x <= 7),
    )))(input)
}

fn parse_extension_field(input: &[u8]) -> IResult<&[u8], ExtensionField> {
    let (input, (field_type, length)) = tuple((be_u16, be_u16))(input)?;
    let (input, value) = nom::bytes::complete::take(length as usize)(input)?;
    let padding_len = (4 - (length % 4)) % 4;
    let (input, _) = nom::bytes::complete::take(padding_len)(input)?;
    Ok((input, ExtensionField {
        field_type,
        length,
        value: value.to_vec(),
    }))
}

fn parse_mac(input: &[u8]) -> IResult<&[u8], MAC> {
    let (input, key_identifier) = be_u32(input)?;
    let (input, message_digest) = nom::bytes::complete::take(20usize)(input)?;
    Ok((input, MAC {
        key_identifier,
        message_digest: message_digest.to_vec(),
    }))
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPv4Packet> {
    let (input, (li, vn, mode)) = parse_first_byte(input)?;
    let (input, (stratum, poll, precision)) = tuple((be_u8, be_i8, be_i8))(input)?;
    let (input, (root_delay, root_dispersion, reference_id)) =
        tuple((be_i32, be_u32, be_u32))(input)?;
    let (input, (reference_timestamp, origin_timestamp, receive_timestamp, transmit_timestamp)) =
        tuple((be_u64, be_u64, be_u64, be_u64))(input)?;
    let (input, extensions) = many0(parse_extension_field)(input)?;
    let (input, mac) = if input.len() >= 24 {
        let (input, mac) = parse_mac(input)?;
        (input, Some(mac))
    } else {
        (input, None)
    };

    Ok((
        input,
        NTPv4Packet {
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
        },
    ))
}

fn main() -> std::io::Result<()> {
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
            println!("Successfully parsed NTP packet:");
            println!("{:#?}", packet);
            if !remaining.is_empty() {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
        }
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}