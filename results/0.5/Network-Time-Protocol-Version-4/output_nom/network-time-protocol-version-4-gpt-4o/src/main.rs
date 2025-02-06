use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_i8, be_u32, be_u8, be_u64},
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
    root_delay: f32,
    root_dispersion: f32,
    reference_identifier: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    key_identifier: Option<u32>,
    message_digest: Option<[u8; 16]>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = be_u8(input)?;
    let li = li_vn_mode >> 6;
    let vn = (li_vn_mode >> 3) & 0x07;
    let mode = li_vn_mode & 0x07;
    let (input, stratum) = be_u8(input)?;
    let (input, poll) = be_i8(input)?;
    let (input, precision) = be_i8(input)?;
    let (input, root_delay) = map_res(be_u32, |x| -> Result<f32, ()> { Ok((x as f32) / (1 << 16) as f32) })(input)?;
    let (input, root_dispersion) = map_res(be_u32, |x| -> Result<f32, ()> { Ok((x as f32) / (1 << 16) as f32) })(input)?;
    let (input, reference_identifier) = be_u32(input)?;
    let (input, reference_timestamp) = be_u64(input)?;
    let (input, originate_timestamp) = be_u64(input)?;
    let (input, receive_timestamp) = be_u64(input)?;
    let (input, transmit_timestamp) = be_u64(input)?;

    let mut key_identifier = None;
    let mut message_digest = None;

    if input.len() >= 4 {
        let (input_new, key_id) = be_u32(input)?;
        key_identifier = Some(key_id);
        if input_new.len() >= 16 {
            let (input_new, md) = take(16usize)(input_new)?;
            let mut md_array = [0u8; 16];
            md_array.copy_from_slice(md);
            message_digest = Some(md_array);
        }
    }

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
            reference_identifier,
            reference_timestamp,
            originate_timestamp,
            receive_timestamp,
            transmit_timestamp,
            key_identifier,
            message_digest,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse NTP packet: {:?}", e),
    }

    Ok(())
}