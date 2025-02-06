use nom::{
    number::complete::{be_i8, be_u8, be_u32, be_u64},
    combinator::opt,
    bytes::complete::take,
    error::{ParseError},
    IResult,
};
use std::{
    fs::File,
    io::{self, Read},
    env,
};

#[derive(Debug)]
struct NTPPacket {
    li: u8,
    vn: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_identifier: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    key_identifier: Option<u32>,
    message_digest: Option<Vec<u8>>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    let (input, first_byte) = be_u8(input)?;
    let li = first_byte >> 6;
    let vn = (first_byte & 0x38) >> 3;
    let mode = first_byte & 0x07;
    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_i8(input)?;
    let (input, precision) = be_i8(input)?;

    let (input, root_delay) = be_u32(input)?;
    let root_delay = (root_delay as f32) / (1 << 16) as f32;
    let (input, root_dispersion) = be_u32(input)?;
    let root_dispersion = (root_dispersion as f32) / (1 << 16) as f32;

    let (input, reference_identifier) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let (input, key_identifier) = opt(be_u32)(input)?;
    let (input, message_digest) = opt(parse_optional_message_digest)(input)?;

    Ok((input, NTPPacket {
        li,
        vn,
        mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_identifier,
        reference_timestamp,
        originate_timestamp,
        receive_timestamp,
        transmit_timestamp,
        key_identifier,
        message_digest,
    }))
}

fn parse_optional_message_digest(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    if input.len() >= 16 {
        let (input, digest) = take(16usize)(input)?;
        Ok((input, digest.to_vec()))
    } else {
        Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Complete)))
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Usage: ntp_parser <filename>"));
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    match parse_ntp_packet(&data) {
        Ok((_rest, packet)) => {
            println!("Parsed NTP Packet: {:?}", packet);
        },
        Err(e) => {
            println!("Failed to parse NTP Packet: {:?}", e);
        }
    }

    Ok(())
}