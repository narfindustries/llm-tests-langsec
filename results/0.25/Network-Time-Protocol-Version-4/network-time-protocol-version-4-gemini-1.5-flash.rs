use nom::{
    bytes::complete::take,
    combinator::{map, map_res},
    number::complete::{be_u32, be_u64, be_u8},
    IResult,
};
use std::fs;
use std::str::from_utf8;

#[derive(Debug)]
struct NtpPacket {
    li: u8,
    vn: u8,
    mode: u8,
    stratum: u8,
    poll: u8,
    precision: u8,
    root_delay: f64,
    root_dispersion: f64,
    reference_id: String,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extension: Vec<u8>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = take(1usize)(input)?;
    let li = (li_vn_mode[0] >> 6) as u8;
    let vn = (li_vn_mode[0] >> 3) & 0b111;
    let mode = li_vn_mode[0] & 0b111;

    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, root_delay) = map(be_u32, |x| x as f64 / 65536.0)(input)?;
    let (input, root_dispersion) = map(be_u32, |x| x as f64 / 65536.0)(input)?;
    let (input, reference_id) = map_res(take(4usize), |x| {
        from_utf8(x).map(|id| id.to_string())
    })(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;
    let (input, extension) = take(input.len())(input)?;

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
            extension: extension.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let contents = fs::read(filename).expect("Failed to read file");

    match parse_ntp_packet(&contents) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Error parsing NTP packet: {:?}", e),
    }
}
