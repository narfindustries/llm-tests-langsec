use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_f32, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::net::Ipv4Addr;

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version_number: u8,
    mode: u8,
    stratum: u8,
    poll: u8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, (li_vn_mode, stratum, poll, precision)) = tuple((be_u8, be_u8, be_u8, be_u8))(input)?;
    let (input, root_delay) = be_f32(input)?;
    let (input, root_dispersion) = be_f32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = map_res(take(8usize), |bytes: &[u8]| {
        Ok(u64::from_be_bytes(bytes.try_into().unwrap()))
    })(input)?;
    let (input, originate_timestamp) = map_res(take(8usize), |bytes: &[u8]| {
        Ok(u64::from_be_bytes(bytes.try_into().unwrap()))
    })(input)?;
    let (input, receive_timestamp) = map_res(take(8usize), |bytes: &[u8]| {
        Ok(u64::from_be_bytes(bytes.try_into().unwrap()))
    })(input)?;
    let (input, transmit_timestamp) = map_res(take(8usize), |bytes: &[u8]| {
        Ok(u64::from_be_bytes(bytes.try_into().unwrap()))
    })(input)?;

    let leap_indicator = (li_vn_mode >> 6) & 0b11;
    let version_number = (li_vn_mode >> 3) & 0b111;
    let mode = li_vn_mode & 0b111;

    Ok((
        input,
        NtpPacket {
            leap_indicator,
            version_number,
            mode,
            stratum,
            poll,
            precision: precision as i8,
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
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("{:?}", packet);
        }
        Err(e) => {
            eprintln!("Failed to parse NTP packet: {:?}", e);
        }
    }
}