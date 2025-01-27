use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    error::Error,
    multi::count,
    number::complete::{be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll_interval: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extensions: Option<Vec<NtpExtension>>,
}

#[derive(Debug)]
struct NtpExtension {
    field_type: u16,
    length: u16,
    data: Vec<u8>,
}

fn parse_ntp_header(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    map(
        tuple((
            take(1usize),
            be_u32,
            be_u32,
            be_u32,
            be_u32,
            be_u32,
            be_u32,
            be_u32,
            opt(parse_extensions),
        )),
        |(first_byte, root_delay, root_dispersion, reference_id, ref_ts, origin_ts, recv_ts, trans_ts, extensions)| {
            let leap_indicator = (first_byte[0] & 0b11000000) >> 6;
            let version = (first_byte[0] & 0b00111000) >> 3;
            let mode = first_byte[0] & 0b00000111;

            NtpPacket {
                leap_indicator,
                version,
                mode,
                stratum: 0,
                poll_interval: 0,
                precision: 0,
                root_delay: f32::from_bits(root_delay),
                root_dispersion: f32::from_bits(root_dispersion),
                reference_id,
                reference_timestamp: ref_ts,
                origin_timestamp: origin_ts,
                receive_timestamp: recv_ts,
                transmit_timestamp: trans_ts,
                extensions,
            }
        },
    )(input)
}

fn parse_extensions(input: &[u8]) -> IResult<&[u8], Vec<NtpExtension>> {
    let (input, _) = tag(&[0, 0, 0, 0])(input)?;
    count(parse_extension, 1)(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], NtpExtension> {
    map(
        tuple((be_u16, be_u16, take_while!(|_| true))),
        |(field_type, length, data)| NtpExtension {
            field_type,
            length,
            data: data.to_vec(),
        },
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_header(&buffer) {
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