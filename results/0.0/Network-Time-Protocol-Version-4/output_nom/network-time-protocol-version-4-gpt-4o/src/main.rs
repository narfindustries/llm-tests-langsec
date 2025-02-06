use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_f32, be_u32, be_u8, be_i8, be_u64},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct NtpPacket {
    li: u8,
    vn: u8,
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
    authenticator: Option<(u32, u64)>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = be_u8(input)?;
    let li = (li_vn_mode >> 6) & 0b11;
    let vn = (li_vn_mode >> 3) & 0b111;
    let mode = li_vn_mode & 0b111;

    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_i8(input)?;
    let (input, root_delay) = be_f32(input)?;
    let (input, root_dispersion) = be_f32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let authenticator = if input.len() >= 12 {
        let (input, key_identifier) = be_u32(input)?;
        let (input, message_digest) = be_u64(input)?;
        Some((key_identifier, message_digest))
    } else {
        None
    };

    Ok((
        input,
        NtpPacket {
            li,
            vn,
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
            authenticator,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(err) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", err);
        return;
    }

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(err) => eprintln!("Error parsing NTP packet: {:?}", err),
    }
}