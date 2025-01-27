use std::env;
use std::fs::File;
use std::io::Read;
use nom::bytes::complete::{tag, take};
use nom::combinator::{map, opt};
use nom::number::complete::{be_u8, be_u16, be_u32};
use nom::sequence::{tuple, preceded};
use nom::IResult;

#[derive(Debug)]
struct NtpHeader {
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
}

#[derive(Debug)]
struct NtpExtension {
    field_type: u16,
    field_length: u16,
    data: Vec<u8>,
}

fn parse_ntp_header(input: &[u8]) -> IResult<&[u8], NtpHeader> {
    map(
        tuple((
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u32,
            be_u32,
            be_u32,
            be_u64,
            be_u64,
            be_u64,
            be_u64,
        )),
        |(
            leap_version_mode,
            poll,
            precision,
            delay,
            dispersion,
            identifier,
            reference_timestamp,
            originate_timestamp,
            receive_timestamp,
            transmit_timestamp,
        )| {
            let leap = (leap_version_mode >> 6) & 0x3;
            let version = (leap_version_mode >> 3) & 0x7;
            let mode = leap_version_mode & 0x7;
            NtpHeader {
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
            }
        },
    )(input)
}

fn parse_ntp_extension(input: &[u8]) -> IResult<&[u8], NtpExtension> {
    map(
        tuple((be_u16, be_u16, take)),
        |(field_type, field_length, data)| NtpExtension {
            field_type,
            field_length,
            data: data.to_vec(),
        },
    )(input)
}

fn parse_ntp_extensions(input: &[u8]) -> IResult<&[u8], Vec<NtpExtension>> {
    let mut extensions = Vec::new();
    let mut remaining = input;
    while !remaining.is_empty() {
        match parse_ntp_extension(remaining) {
            Ok((remaining_input, extension)) => {
                extensions.push(extension);
                remaining = remaining_input;
            }
            Err(_) => break,
        }
    }
    Ok((remaining, extensions))
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], (NtpHeader, Vec<NtpExtension>)> {
    map(
        tuple((parse_ntp_header, opt(parse_ntp_extensions))),
        |(header, extensions)| (header, extensions.unwrap_or_default()),
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let mut file = File::open(&args[1]).expect("Failed to open input file");
    let mut input = Vec::new();
    file.read_to_end(&mut input).expect("Failed to read input file");
    match parse_ntp_packet(&input) {
        Ok((remaining, (header, extensions))) => {
            println!("Header: {:?}", header);
            println!("Extensions: {:?}", extensions);
            println!("Remaining: {:?}", remaining);
        }
        Err(err) => println!("Error: {:?}", err),
    }
}