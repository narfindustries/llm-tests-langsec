use nom::{
    bytes::complete::take,
    number::complete::{be_i32, be_u32, be_u64},
    IResult, error::{Error, ErrorKind},
    Err,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NtpPacket {
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
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    key_identifier: Option<u32>,
    message_digest: Option<[u8; 16]>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket, Error<ErrorKind>> {
    let (input, li_vn_mode) = take(1usize)(input)?;
    let li = (li_vn_mode[0] >> 6) & 0b11;
    let vn = (li_vn_mode[0] >> 3) & 0b111;
    let mode = li_vn_mode[0] & 0b111;

    let (input, stratum) = take(1usize)(input)?;
    let stratum = stratum[0];

    let (input, poll) = take(1usize)(input)?;
    let poll = poll[0];

    let (input, precision) = take(1usize)(input)?;
    let precision = precision[0] as i8;

    let (input, root_delay) = be_i32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let (input, key_identifier) = match be_u32(input) {
        Ok((i, key)) => (i, Some(key)),
        Err(_) => (input, None),
    };

    let (input, message_digest) = match take(16usize)(input) {
        Ok((i, digest)) => (i, Some(digest.try_into().unwrap())),
        Err(_) => (input, None),
    };

    Ok((
        input,
        NtpPacket {
            li: li,
            vn: vn,
            mode: mode,
            stratum: stratum,
            poll: poll,
            precision: precision,
            root_delay: root_delay,
            root_dispersion: root_dispersion,
            reference_id: reference_id,
            reference_timestamp: reference_timestamp,
            originate_timestamp: originate_timestamp,
            receive_timestamp: receive_timestamp,
            transmit_timestamp: transmit_timestamp,
            key_identifier: key_identifier,
            message_digest: message_digest,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => println!("Error parsing NTP packet: {:?}", e),
    }
}
