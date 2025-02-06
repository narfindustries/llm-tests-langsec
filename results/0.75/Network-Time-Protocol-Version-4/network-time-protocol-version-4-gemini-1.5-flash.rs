use nom::{
    bytes::complete::take,
    number::complete::{be_i8, be_u16, be_u32, be_u64, be_u8},
    IResult,
};
use std::env;
use std::fs::read;

#[derive(Debug)]
struct NtpPacket {
    li: u8,
    vn: u8,
    mode: u8,
    stratum: u8,
    poll: u8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_id: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
}

fn ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = take(1usize)(input)?;
    let li = (li_vn_mode[0] >> 6) as u8;
    let vn = (li_vn_mode[0] >> 3) & 0b111;
    let mode = li_vn_mode[0] & 0b111;

    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_i8(input)?;
    let (input, root_delay) = be_u32(input)?;
    let (input, root_dispersion) = be_u32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

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
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = match read(filename) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        }
    };

    match ntp_packet(&data) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Error parsing NTP packet: {:?}", e),
    }
}
