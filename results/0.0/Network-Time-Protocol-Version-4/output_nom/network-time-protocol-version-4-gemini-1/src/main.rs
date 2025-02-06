use nom::{
    bytes::complete::take,
    combinator::{map, map_res},
    number::complete::be_u8,
    IResult,
};
use std::fs::read;

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
    reference_id: [u8; 4],
    reference_timestamp: i64,
    originate_timestamp: i64,
    receive_timestamp: i64,
    transmit_timestamp: i64,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = take(1usize)(input)?;
    let li = (li_vn_mode[0] >> 6) as u8;
    let vn = (li_vn_mode[0] >> 3) & 0b111;
    let mode = li_vn_mode[0] & 0b111;

    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, root_delay_bytes) = take(4usize)(input)?;
    let root_delay = {
        let root_delay_u32 = u32::from_be_bytes(root_delay_bytes.try_into().unwrap());
        f64::from_bits(root_delay_u32 as u64)
    };
    let (input, root_dispersion_bytes) = take(4usize)(input)?;
    let root_dispersion = {
        let root_dispersion_u32 = u32::from_be_bytes(root_dispersion_bytes.try_into().unwrap());
        f64::from_bits(root_dispersion_u32 as u64)
    };
    let (input, reference_id) = take(4usize)(input)?;
    let (input, reference_timestamp_bytes) = take(8usize)(input)?;
    let reference_timestamp = {
        let ts_u64 = u64::from_be_bytes(reference_timestamp_bytes.try_into().unwrap());
        ts_u64 as i64
    };
    let (input, originate_timestamp_bytes) = take(8usize)(input)?;
    let originate_timestamp = {
        let ts_u64 = u64::from_be_bytes(originate_timestamp_bytes.try_into().unwrap());
        ts_u64 as i64
    };
    let (input, receive_timestamp_bytes) = take(8usize)(input)?;
    let receive_timestamp = {
        let ts_u64 = u64::from_be_bytes(receive_timestamp_bytes.try_into().unwrap());
        ts_u64 as i64
    };
    let (input, transmit_timestamp_bytes) = take(8usize)(input)?;
    let transmit_timestamp = {
        let ts_u64 = u64::from_be_bytes(transmit_timestamp_bytes.try_into().unwrap());
        ts_u64 as i64
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
            reference_id: reference_id.try_into().unwrap(),
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

    let filename = &args[1];
    let data = match read(filename) {
        Ok(d) => d,
        Err(e) => {
            eprintln!("Error reading file {}: {}", filename, e);
            std::process::exit(1);
        }
    };

    match parse_ntp_packet(&data) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => {
            eprintln!("Error parsing NTP packet: {:?}", e);
            std::process::exit(1);
        }
    }
}
