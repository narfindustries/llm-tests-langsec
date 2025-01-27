use nom::{
    number::complete::{be_u8, be_u32},
    IResult,
};
use std::{env, fs::File, io::{self, Read}};

#[derive(Debug)]
struct NtpPacket {
    li_vn_mode: u8,
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
    extension_data: Option<Vec<u8>>,
    key_identifier: Option<u32>,
    message_digest: Option<Vec<u8>>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = be_u8(input)?;
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

    let mut extension_data = None;
    let mut key_identifier = None;
    let mut message_digest = None;
    
    let extension_present = input.len() > 0;
    if extension_present {
        let (input, ext_data) = nom::bytes::complete::take(input.len() - 20)(input)?;
        let (input, key_id) = be_u32(input)?;
        let (input, digest) = nom::bytes::complete::take(16usize)(input)?;

        extension_data = Some(ext_data.to_vec());
        key_identifier = Some(key_id);
        message_digest = Some(digest.to_vec());

        Ok((input, NtpPacket {
            li_vn_mode,
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
            extension_data,
            key_identifier,
            message_digest,
        }))
    } else {
        Ok((input, NtpPacket {
            li_vn_mode,
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
            extension_data,
            key_identifier,
            message_digest,
        }))
    }
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