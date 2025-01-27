use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::take,
    number::complete::{be_u8, be_u16, be_u32, be_u64},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug)]
struct NtpPacket {
    leap: u8,
    version: u8,
    mode: u8,
    poll: u8,
    precision: u8,
    delay: u32,
    dispersion: u32,
    identifier: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extension_fields: Vec<NtpExtensionField>,
}

#[derive(Debug)]
struct NtpExtensionField {
    field_type: u16,
    field_length: u16,
    data: Vec<u8>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, leap) = be_u8(input)?;
    let (input, version) = be_u8(input)?;
    let (input, mode) = be_u8(input)?;
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, delay) = be_u32(input)?;
    let (input, dispersion) = be_u32(input)?;
    let (input, identifier) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let (input, extension_fields) = parse_ntp_extension_fields(input)?;

    Ok((input, NtpPacket {
        leap,
        version,
        mode,
        poll,
        precision,
        delay,
        dispersion,
        identifier,
        reference_timestamp,
        originate_timestamp,
        receive_timestamp,
        transmit_timestamp,
        extension_fields,
    }))
}

fn parse_ntp_extension_fields(input: &[u8]) -> IResult<&[u8], Vec<NtpExtensionField>> {
    let mut extension_fields = Vec::new();
    let mut input = input;

    while !input.is_empty() {
        let (input_remaining, field_type) = be_u16(input)?;
        input = input_remaining;
        let (input_remaining, field_length) = be_u16(input)?;
        input = input_remaining;
        let (input_remaining, data) = take(field_length as usize)(input)?;
        input = input_remaining;

        extension_fields.push(NtpExtensionField {
            field_type,
            field_length,
            data: data.to_vec(),
        });
    }

    Ok((input, extension_fields))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }

    let input_file = &args[1];
    let path = Path::new(input_file);
    let file = File::open(path).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).expect("Failed to read file");

    let result = parse_ntp_packet(&input);
    match result {
        Ok((remaining, packet)) => {
            if !remaining.is_empty() {
                panic!("Unexpected remaining input");
            }
            println!("{:?}", packet);
        }
        Err(err) => {
            panic!("Failed to parse NTP packet: {:?}", err);
        }
    }
}