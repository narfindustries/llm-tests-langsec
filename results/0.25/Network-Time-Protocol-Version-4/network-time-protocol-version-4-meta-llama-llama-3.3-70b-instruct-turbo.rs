use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::take,
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug, PartialEq)]
struct NtpPacket {
    flags: u8,
    stratum: u8,
    poll: u8,
    precision: u8,
    delay: u32,
    dispersion: u32,
    identifier: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extension_fields: Vec<(u16, u16, Vec<u8>)>,
}

fn parse_ntp_flags(input: &[u8]) -> IResult<&[u8], u8> {
    map(be_u8, |x: u8| x >> 6)(input)
}

fn parse_ntp_version(input: &[u8]) -> IResult<&[u8], u8> {
    map(be_u8, |x: u8| (x >> 3) & 0x7)(input)
}

fn parse_ntp_mode(input: &[u8]) -> IResult<&[u8], u8> {
    map(be_u8, |x: u8| x & 0x7)(input)
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    map(
        tuple((
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
            leap_second,
            version,
            mode,
            stratum,
            poll,
            precision,
            delay,
            dispersion,
            identifier,
            reference_timestamp,
            origin_timestamp,
            receive_timestamp,
            transmit_timestamp,
        ): (
            u8,
            u8,
            u8,
            u8,
            u8,
            u8,
            u32,
            u32,
            u32,
            u64,
            u64,
            u64,
            u64,
        )| {
            let flags = (leap_second << 6) | (version << 3) | mode;
            let extension_fields = vec![];
            NtpPacket {
                flags,
                stratum,
                poll,
                precision,
                delay,
                dispersion,
                identifier,
                reference_timestamp,
                origin_timestamp,
                receive_timestamp,
                transmit_timestamp,
                extension_fields,
            }
        },
    )(input)
}

fn parse_ntp_extension_field(input: &[u8]) -> IResult<&[u8], (u16, u16, Vec<u8>)> {
    map(
        tuple((be_u16, be_u16, take)),
        |(field_type, field_length, field_value): (u16, u16, Vec<u8>)| {
            (field_type, field_length, field_value)
        },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let input_file = &args[1];
    let mut file = BufReader::new(File::open(&Path::new(input_file)).unwrap());
    let mut input = Vec::new();
    file.read_to_end(&mut input).unwrap();
    let result = parse_ntp_packet(&input);
    match result {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(err) => panic!("Error parsing NTP packet: {:?}", err),
    }
}