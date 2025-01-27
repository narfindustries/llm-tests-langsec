use nom::{
    bits::{bits, complete::take},
    bytes::complete::take as take_bytes,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct NtpPacket {
    li: u8,
    vn: u8,
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
    let (input, (li_vn_mode, stratum, poll, precision)) = bits::<_, _, nom::error::Error<(&[u8], usize)>, _>((
        take::<_, u8, _, _>(2usize),
        take::<_, u8, _, _>(3usize),
        take::<_, u8, _, _>(3usize),
        take_bytes(1usize),
        take_bytes(1usize),
        take_bytes(1usize),
        take_bytes(1usize),
    ))(input)?;

    let li = li_vn_mode.0;
    let vn = li_vn_mode.1;
    let mode = li_vn_mode.2;

    let (input, root_delay) = take_bytes::<_, _, nom::error::Error<&[u8]>>(4usize)(input)?;
    let root_delay = u32::from_be_bytes(root_delay.try_into().unwrap());

    let (input, root_dispersion) = take_bytes::<_, _, nom::error::Error<&[u8]>>(4usize)(input)?;
    let root_dispersion = u32::from_be_bytes(root_dispersion.try_into().unwrap());

    let (input, reference_id) = take_bytes::<_, _, nom::error::Error<&[u8]>>(4usize)(input)?;
    let reference_id = u32::from_be_bytes(reference_id.try_into().unwrap());

    let (input, reference_timestamp) = take_bytes::<_, _, nom::error::Error<&[u8]>>(8usize)(input)?;
    let reference_timestamp = u64::from_be_bytes(reference_timestamp.try_into().unwrap());

    let (input, originate_timestamp) = take_bytes::<_, _, nom::error::Error<&[u8]>>(8usize)(input)?;
    let originate_timestamp = u64::from_be_bytes(originate_timestamp.try_into().unwrap());

    let (input, receive_timestamp) = take_bytes::<_, _, nom::error::Error<&[u8]>>(8usize)(input)?;
    let receive_timestamp = u64::from_be_bytes(receive_timestamp.try_into().unwrap());

    let (input, transmit_timestamp) = take_bytes::<_, _, nom::error::Error<&[u8]>>(8usize)(input)?;
    let transmit_timestamp = u64::from_be_bytes(transmit_timestamp.try_into().unwrap());

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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse NTP packet: {:?}", e),
    }

    Ok(())
}