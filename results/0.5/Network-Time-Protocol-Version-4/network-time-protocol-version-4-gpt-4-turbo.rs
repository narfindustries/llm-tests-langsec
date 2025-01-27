use nom::{
    bits::complete::{tag, take},
    bytes::complete::take,
    combinator::map,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct NTPPacket {
    leap_indicator: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: u32,
    root_dispersion: u32,
    reference_identifier: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extension_data: Option<Vec<u8>>,
    key_identifier: Option<u32>,
    message_digest: Option<Vec<u8>>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    map(
        tuple((
            take(2usize),
            be_u8,
            be_u8,
            be_u8,
            be_u8,
            be_u32,
            be_u32,
            be_u32,
            be_u64,
            be_u64,
            be_u64,
            be_u64,
            nom::combinator::opt(take(usize::from(input.len() - 48))),
        )),
        |(li_vn_mode, stratum, poll, precision, root_delay, root_dispersion, reference_identifier, reference_timestamp, originate_timestamp, receive_timestamp, transmit_timestamp, extension_data)| {
            let (li, vn, mode) = ((li_vn_mode[0] >> 6) & 0x03, (li_vn_mode[0] >> 3) & 0x07, li_vn_mode[0] & 0x07);
            let extension_data = extension_data.map(|x| x.to_vec());
            NTPPacket {
                leap_indicator: li,
                version: vn,
                mode: mode,
                stratum,
                poll: poll as i8,
                precision: precision as i8,
                root_delay,
                root_dispersion,
                reference_identifier,
                reference_timestamp,
                originate_timestamp,
                receive_timestamp,
                transmit_timestamp,
                extension_data,
                key_identifier: None,
                message_digest: None,
            }
        },
    )(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Failed to parse NTP packet: {:?}", e),
    }

    Ok(())
}