use nom::{
    bits::complete::take,
    combinator::map,
    error::Error,
    multi::count,
    number::complete::{be_i32, be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct NTPTimestamp {
    seconds: u32,
    fraction: u32,
}

#[derive(Debug)]
struct ReferenceIdentifier {
    id: [u8; 4],
}

#[derive(Debug)]
struct NTPPacket {
    li_vn_mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_id: ReferenceIdentifier,
    reference_timestamp: NTPTimestamp,
    origin_timestamp: NTPTimestamp,
    receive_timestamp: NTPTimestamp,
    transmit_timestamp: NTPTimestamp,
    extension_fields: Vec<ExtensionField>,
    mac: Option<Vec<u8>>,
}

#[derive(Debug)]
struct ExtensionField {
    field_type: u16,
    length: u16,
    value: Vec<u8>,
}

fn parse_timestamp(input: &[u8]) -> IResult<&[u8], NTPTimestamp> {
    let (input, (seconds, fraction)) = tuple((be_u32, be_u32))(input)?;
    Ok((
        input,
        NTPTimestamp {
            seconds,
            fraction,
        },
    ))
}

fn parse_reference_id(input: &[u8]) -> IResult<&[u8], ReferenceIdentifier> {
    let (input, id) = count(be_u8, 4)(input)?;
    Ok((
        input,
        ReferenceIdentifier {
            id: id.try_into().unwrap(),
        },
    ))
}

fn parse_extension_field(input: &[u8]) -> IResult<&[u8], ExtensionField> {
    let (input, (field_type, length)) = tuple((be_u16, be_u16))(input)?;
    let value_length = length as usize - 4;
    let (input, value) = count(be_u8, value_length)(input)?;
    Ok((
        input,
        ExtensionField {
            field_type,
            length,
            value,
        },
    ))
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    let (input, (
        li_vn_mode,
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
    )) = tuple((
        be_u8,
        be_u8,
        map(be_i32, |x| x as i8),
        map(be_i32, |x| x as i8),
        be_u32,
        be_u32,
        parse_reference_id,
        parse_timestamp,
        parse_timestamp,
        parse_timestamp,
        parse_timestamp,
    ))(input)?;

    let mut remaining = input;
    let mut extension_fields = Vec::new();
    
    while remaining.len() >= 4 {
        match parse_extension_field(remaining) {
            Ok((new_remaining, field)) => {
                extension_fields.push(field);
                remaining = new_remaining;
            }
            Err(_) => break,
        }
    }

    let mac = if !remaining.is_empty() {
        Some(remaining.to_vec())
    } else {
        None
    };

    Ok((
        &[],
        NTPPacket {
            li_vn_mode,
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
            extension_fields,
            mac,
        },
    ))
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
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Failed to parse NTP packet: {:?}", e),
    }

    Ok(())
}