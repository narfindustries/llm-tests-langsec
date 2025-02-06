use nom::{
    bytes::complete::{take},
    combinator::{map, map_res, opt},
    number::complete::{be_u16, be_u32, be_u64, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
enum ReferenceClockIdentifier {
    Unspecified,
    ReferenceClockType(u8),
    IPv4Address(u32),
    IPv6Address([u8; 16]),
}

impl ReferenceClockIdentifier {
    fn parse(input: &[u8]) -> IResult<&[u8], ReferenceClockIdentifier> {
        map_res(take(4usize), |input: &[u8]| match input {
            [0, 0, 0, 0] => Ok(ReferenceClockIdentifier::Unspecified),
            [a, b, c, d] if *a == 0 && *b == 0 && *c == 0 && *d <= 255 => {
                Ok(ReferenceClockIdentifier::ReferenceClockType(*d))
            }
            [a, b, c, d] if *a == 0 && *b == 0 && *c == 0 && *d > 0 => {
                let ip_address = ((*a as u32) << 24) | ((*b as u32) << 16) | ((*c as u32) << 8) | (*d as u32);
                Ok(ReferenceClockIdentifier::IPv4Address(ip_address))
            }
            [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p] => {
                let ip_address = [
                    *a, *b, *c, *d, *e, *f, *g, *h, *i, *j, *k, *l, *m, *n, *o, *p,
                ];
                Ok(ReferenceClockIdentifier::IPv6Address(ip_address))
            }
            _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
        })(input)
    }
}

#[derive(Debug)]
struct NTPHeader {
    leap_indicator: u8,
    version_number: u8,
    mode: u8,
    poll: u8,
    precision: u8,
    root_delay: u32,
    root_dispersion: u32,
    reference_clock_identifier: ReferenceClockIdentifier,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

impl NTPHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], NTPHeader> {
        let (input, leap_indicator) = map(be_u8, |x: u8| x >> 6)(input)?;
        let (input, version_number) = map(be_u8, |x: u8| (x >> 3) & 0x07)(input)?;
        let (input, mode) = map(be_u8, |x: u8| x & 0x07)(input)?;
        let (input, poll) = be_u8(input)?;
        let (input, precision) = be_u8(input)?;
        let (input, root_delay) = be_u32(input)?;
        let (input, root_dispersion) = be_u32(input)?;
        let (input, reference_clock_identifier) = ReferenceClockIdentifier::parse(input)?;
        let (input, reference_timestamp) = be_u64(input)?;
        let (input, origin_timestamp) = be_u64(input)?;
        let (input, receive_timestamp) = be_u64(input)?;
        let (input, transmit_timestamp) = be_u64(input)?;
        Ok((
            input,
            NTPHeader {
                leap_indicator,
                version_number,
                mode,
                poll,
                precision,
                root_delay,
                root_dispersion,
                reference_clock_identifier,
                reference_timestamp,
                origin_timestamp,
                receive_timestamp,
                transmit_timestamp,
            },
        ))
    }
}

#[derive(Debug)]
enum NTPExtensionField {
    MAC(()),
    Autokey(()),
    NTPv4ExtensionField(()),
}

impl NTPExtensionField {
    fn parse(input: &[u8]) -> IResult<&[u8], NTPExtensionField> {
        let (input, field_type) = be_u16(input)?;
        let (input, field_length) = be_u16(input)?;
        let (input, field_data) = take(field_length as usize)(input)?;
        match field_type {
            0 => Ok((input, NTPExtensionField::MAC(()))),
            1 => Ok((input, NTPExtensionField::Autokey(()))),
            _ => Ok((input, NTPExtensionField::NTPv4ExtensionField(()))),
        }
    }
}

#[derive(Debug)]
struct NTPPacket {
    header: NTPHeader,
    extension_fields: Vec<NTPExtensionField>,
}

impl NTPPacket {
    fn parse(input: &[u8]) -> IResult<&[u8], NTPPacket> {
        let (input, header) = NTPHeader::parse(input)?;
        let mut extension_fields = Vec::new();
        let mut remaining_input = input;
        while !remaining_input.is_empty() {
            let (input, extension_field) = opt(NTPExtensionField::parse)(remaining_input)?;
            match extension_field {
                Some(field) => {
                    extension_fields.push(field);
                    remaining_input = input;
                }
                None => break,
            }
        }
        Ok((
            remaining_input,
            NTPPacket {
                header,
                extension_fields,
            },
        ))
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let file = File::open(input_file).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).expect("Failed to read file");
    match NTPPacket::parse(&input) {
        Ok((remaining, packet)) => {
            println!("Parsed NTP packet: {:?}", packet);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining in input", remaining.len());
            }
        }
        Err(err) => println!("Error parsing NTP packet: {:?}", err),
    }
}