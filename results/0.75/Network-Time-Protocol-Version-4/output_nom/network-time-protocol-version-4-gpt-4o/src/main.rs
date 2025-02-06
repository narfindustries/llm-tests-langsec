use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_u32, be_u64, be_u8},
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
    precision: u8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    key_identifier: Option<u32>,
    message_digest: Option<[u8; 16]>, // Assuming MD5 for simplicity, modify if SHA-1 or other
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = be_u8(input)?;
    let li = (li_vn_mode >> 6) & 0b11;
    let vn = (li_vn_mode >> 3) & 0b111;
    let mode = li_vn_mode & 0b111;

    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_u8(input)?;
    let (input, precision) = be_u8(input)?;
    let (input, root_delay) = map_res(be_u32, |x| -> Result<f32, std::num::TryFromIntError> {
        Ok(x as f32 / (1 << 16) as f32)
    })(input)?;
    let (input, root_dispersion) = map_res(be_u32, |x| -> Result<f32, std::num::TryFromIntError> {
        Ok(x as f32 / (1 << 16) as f32)
    })(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let (input, key_identifier) = if input.len() >= 4 {
        let (input, key_id) = be_u32(input)?;
        (input, Some(key_id))
    } else {
        (input, None)
    };

    let (input, message_digest) = if input.len() >= 16 {
        let (input, md) = take(16u8)(input)?;
        let mut md_array = [0u8; 16];
        md_array.copy_from_slice(md);
        (input, Some(md_array))
    } else {
        (input, None)
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
            key_identifier,
            message_digest,
        },
    ))
}

fn read_file_to_end<P: AsRef<Path>>(path: P) -> Result<Vec<u8>, std::io::Error> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    match read_file_to_end(file_path) {
        Ok(data) => match parse_ntp_packet(&data) {
            Ok((_, packet)) => println!("{:#?}", packet),
            Err(e) => eprintln!("Failed to parse NTP packet: {:?}", e),
        },
        Err(e) => eprintln!("Failed to read file: {:?}", e),
    }
}