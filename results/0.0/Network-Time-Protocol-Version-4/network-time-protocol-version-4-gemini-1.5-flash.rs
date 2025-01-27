use nom::{
    be::{be_u16, be_u32, be_u8},
    bytes::complete::take,
    combinator::{map, map_res, opt, verify},
    error::ErrorKind,
    multi::count,
    number::complete::le_u32,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::net::{IpAddr, Ipv4Addr};
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
}


fn parse_ntp_timestamp(input: &[u8]) -> IResult<&[u8], NtpTimestamp> {
    let (input, seconds) = be_u32(input)?;
    let (input, fraction) = be_u32(input)?;
    Ok((input, NtpTimestamp { seconds, fraction }))
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = be_u8(input)?;
    let (input, stratum) = be_u8(input)?;
    let (input, poll) = map_res(be_i8, |x| {
        if x >= -16 && x <= 7 {
            Ok(x)
        } else {
            Err(nom::Err::Failure(nom::error::Error::new(input, ErrorKind::Verify)))
        }
    })(input)?;
    let (input, precision) = map_res(be_i8, |x| {
        if x >= -16 && x <= 7 {
            Ok(x)
        } else {
            Err(nom::Err::Failure(nom::error::Error::new(input, ErrorKind::Verify)))
        }
    })(input)?;
    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = parse_ntp_timestamp(input)?;
    let (input, originate_timestamp) = parse_ntp_timestamp(input)?;
    let (input, receive_timestamp) = parse_ntp_timestamp(input)?;
    let (input, transmit_timestamp) = parse_ntp_timestamp(input)?;

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
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            std::process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            std::process::exit(1);
        }
    };

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Error parsing NTP packet: {:?}", e),
    }
}
