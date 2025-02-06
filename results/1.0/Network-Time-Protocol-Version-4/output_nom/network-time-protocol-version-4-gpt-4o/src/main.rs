use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    IResult, bytes::complete::take, number::complete::{be_u8, be_i8, be_u32, be_u64}
};

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version_number: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_id: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = be_u8(input)?;
    let leap_indicator = (li_vn_mode >> 6) & 0x03;
    let version_number = (li_vn_mode >> 3) & 0x07;
    let mode = li_vn_mode & 0x07;

    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_i8(input)?;
    let (input, precision) = be_i8(input)?;
    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    Ok((input, NtpPacket {
        leap_indicator,
        version_number,
        mode,
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
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_remaining, ntp_packet)) => println!("{:?}", ntp_packet),
        Err(e) => eprintln!("Failed to parse NTP packet: {:?}", e),
    }

    Ok(())
}