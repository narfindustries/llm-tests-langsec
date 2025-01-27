use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::count,
    number::complete::{be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version: u8,
    mode: u8,
    stratum: u8,
    poll_interval: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: u32,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    extensions: Option<Vec<NtpExtension>>,
    authentication_data: Option<Vec<u8>>,
}

#[derive(Debug)]
struct NtpExtension {
    field_type: u16,
    length: u16,
    data: Vec<u8>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    map(
        tuple((
            take(1usize),
            opt(count(take(4usize), 5)),
            opt(take(16usize)),
        )),
        |(header, extensions, auth_data)| {
            let first_byte = header[0];
            NtpPacket {
                leap_indicator: (first_byte >> 6) & 0b11,
                version: (first_byte >> 3) & 0b111,
                mode: first_byte & 0b111,
                stratum: 0,
                poll_interval: 0,
                precision: 0,
                root_delay: 0.0,
                root_dispersion: 0.0,
                reference_id: 0,
                reference_timestamp: 0,
                originate_timestamp: 0,
                receive_timestamp: 0,
                transmit_timestamp: 0,
                extensions: extensions.map(|ext| {
                    ext.iter()
                        .map(|e| NtpExtension {
                            field_type: 0,
                            length: 0,
                            data: e.to_vec(),
                        })
                        .collect()
                }),
                authentication_data: auth_data.map(|a| a.to_vec()),
            }
        },
    )(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => {
            println!("Parsed NTP Packet: {:?}", packet);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }
}