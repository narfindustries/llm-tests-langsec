use nom::bits::complete::take as take_bits;
use nom::error::Error;
use nom::IResult;
use nom::sequence::tuple;
use nom::combinator::map;
use nom::number::complete::{be_i32, be_u32, be_u8, i8 as be_i8};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
pub struct NtpPacket {
    li: u8,
    vn: u8,
    mode: u8,
    stratum: u8,
    poll: u8,
    precision: i8,
    root_delay: i32,
    root_dispersion: u32,
    reference_id: u32,
    reference_timestamp: u64,
    origin_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    key_identifier: Option<u32>,
    message_digest: Option<Vec<u8>>
}

fn parse_first_byte(input: (&[u8], usize)) -> IResult<(&[u8], usize), (u8, u8, u8)> {
    tuple((
        take_bits(2u8),  // LI
        take_bits(3u8),  // VN
        take_bits(3u8)   // Mode
    ))(input)
}

fn parse_timestamps(input: &[u8]) -> IResult<&[u8], u64> {
    map(tuple((
        be_u32,
        be_u32
    )), |(seconds, fraction)| {
        ((seconds as u64) << 32) | (fraction as u64)
    })(input)
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, (li, vn, mode)) = nom::bits::bits::<_, _, Error<(&[u8], usize)>, _, _>(
        parse_first_byte
    )(input)?;

    let (input, (
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_ts,
        origin_ts,
        receive_ts,
        transmit_ts
    )) = tuple((
        be_u8,
        be_u8,
        be_i8,
        be_i32,
        be_u32,
        be_u32,
        parse_timestamps,
        parse_timestamps,
        parse_timestamps,
        parse_timestamps
    ))(input)?;

    // Optional authentication fields
    let (input, key_id) = if input.len() >= 4 {
        let (i, kid) = be_u32(input)?;
        (i, Some(kid))
    } else {
        (input, None)
    };

    let message_digest = if input.len() >= 16 {
        Some(input[..16].to_vec())
    } else {
        None
    };

    Ok((input, NtpPacket {
        li,
        vn,
        mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp: reference_ts,
        origin_timestamp: origin_ts,
        receive_timestamp: receive_ts,
        transmit_timestamp: transmit_ts,
        key_identifier: key_id,
        message_digest
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <ntp_packet_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed NTP packet: {:#?}", packet);
            if !remaining.is_empty() {
                println!("Warning: {} bytes remaining", remaining.len());
            }
        },
        Err(e) => println!("Error parsing NTP packet: {:?}", e),
    }

    Ok(())
}