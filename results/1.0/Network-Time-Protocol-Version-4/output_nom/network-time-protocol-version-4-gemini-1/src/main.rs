use nom::{
    be::{be_u16, be_u32, be_u8},
    bytes::complete::take,
    combinator::{map, map_res, opt},
    error::ErrorKind,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::net::Ipv4Addr;
use std::path::Path;

#[derive(Debug, PartialEq)]
struct NtpTimestamp {
    seconds: u32,
    fraction: u32,
}

#[derive(Debug, PartialEq)]
struct NtpPacket {
    li_vn_mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_id: u32,
    reference_timestamp: NtpTimestamp,
    originate_timestamp: NtpTimestamp,
    receive_timestamp: NtpTimestamp,
    transmit_timestamp: NtpTimestamp,
    // Optional fields
    extension: Option<Vec<u8>>,
}

fn ntp_timestamp(input: &[u8]) -> IResult<&[u8], NtpTimestamp> {
    let (input, seconds) = be_u32(input)?;
    let (input, fraction) = be_u32(input)?;
    Ok((input, NtpTimestamp { seconds, fraction }))
}

fn ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = be_u8(input)?;
    let (input, stratum) = be_u8(input)?;
    let (input, poll) = map_res(be_u8(input), |x| x.wrapping_sub(128) as i8)(input)?;
    let (input, precision) = map_res(be_u8(input), |x| x.wrapping_sub(128) as i8)(input)?;
    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = ntp_timestamp(input)?;
    let (input, originate_timestamp) = ntp_timestamp(input)?;
    let (input, receive_timestamp) = ntp_timestamp(input)?;
    let (input, transmit_timestamp) = ntp_timestamp(input)?;
    let (input, extension) = opt(take(48usize))(input)?;

    Ok((
        input,
        NtpPacket {
            li_vn_mode,
            stratum,
            poll,
            precision,
            root_delay,
            root_dispersion,
            reference_id,
            reference_timestamp,
            originate_timestamp,
            receive_timestamp,
            transmit_timestamp,
            extension,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            std::process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            std::process::exit(1);
        }
    };

    match ntp_packet(&buffer) {
        Ok((leftover, packet)) => {
            println!("Parsed NTP Packet:\n{:#?}", packet);
            if !leftover.is_empty() {
                println!("Leftover bytes: {:?}", leftover);
            }
        }
        Err(e) => {
            println!("Error parsing NTP packet: {:?}", e);
        }
    }
}
