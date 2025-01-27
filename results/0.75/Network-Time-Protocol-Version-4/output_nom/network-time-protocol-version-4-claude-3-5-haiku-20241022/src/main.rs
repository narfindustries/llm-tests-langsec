use nom::{
    bits::{complete::take as take_bits, streaming::take as take_streaming_bits},
    bytes::complete::{tag, take},
    error::ErrorKind,
    multi::count,
    number::complete::{be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult, Parser,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NTPPacket {
    flags: u8,
    stratum: u8,
    poll_interval: i8,
    precision: i8,
    root_delay: u16,
    root_dispersion: u16,
    reference_id: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extensions: Option<Vec<NTPExtension>>,
}

#[derive(Debug)]
struct NTPExtension {
    field_type: u16,
    length: u16,
    data: Vec<u8>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    let (input, (
        flags,
        stratum,
        poll_interval,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp
    )) = tuple((
        take(1u8),
        take(1u8),
        take(1u8),
        take(1u8),
        be_u16,
        be_u16,
        be_u32,
        be_u64,
        be_u64,
        be_u64,
        be_u64
    ))(input)?;

    let (input, extensions) = parse_extensions(input)?;

    Ok((input, NTPPacket {
        flags: flags[0],
        stratum,
        poll_interval: poll_interval as i8,
        precision: precision as i8,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp,
        extensions,
    }))
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Option<Vec<NTPExtension>>> {
    let mut extensions = Vec::new();
    let mut remaining = input;

    while !remaining.is_empty() {
        let (rest, field_type) = be_u16(remaining)?;
        if field_type == 0 {
            break;
        }

        let (rest, length) = be_u16(rest)?;
        let (rest, data) = take(length - 4)(rest)?;

        extensions.push(NTPExtension {
            field_type,
            length,
            data: data.to_vec(),
        });

        remaining = rest;
    }

    Ok((remaining, if extensions.is_empty() { None } else { Some(extensions) }))
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

    match parse_ntp_packet(&buffer) {
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