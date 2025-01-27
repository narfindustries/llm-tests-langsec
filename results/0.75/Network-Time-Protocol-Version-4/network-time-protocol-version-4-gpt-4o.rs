use nom::{
    bytes::complete::take,
    combinator::map_res,
    number::complete::{be_f32, be_i32, be_u32, be_u8},
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct NtpPacket {
    leap_indicator: u8,
    version_number: u8,
    mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: u32,
    reference_timestamp: u32,
    originate_timestamp: u32,
    receive_timestamp: u32,
    transmit_timestamp: u32,
    optional_extensions: Option<Vec<u8>>,
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (input, li_vn_mode) = be_u8(input)?;
    let leap_indicator = (li_vn_mode >> 6) & 0x03;
    let version_number = (li_vn_mode >> 3) & 0x07;
    let mode = li_vn_mode & 0x07;

    let (input, stratum) = be_u8(input)?;
    let (input, poll) = map_res(be_u8, |v| Ok(v as i8))(input)?;
    let (input, precision) = map_res(be_u8, |v| Ok(v as i8))(input)?;

    let (input, root_delay) = be_f32(input)?;
    let (input, root_dispersion) = be_f32(input)?;
    let (input, reference_id) = be_u32(input)?;
    let (input, reference_timestamp) = be_u32(input)?;
    let (input, originate_timestamp) = be_u32(input)?;
    let (input, receive_timestamp) = be_u32(input)?;
    let (input, transmit_timestamp) = be_u32(input)?;

    let optional_extensions = if !input.is_empty() {
        Some(input.to_vec())
    } else {
        None
    };

    Ok((
        &[],
        NtpPacket {
            leap_indicator,
            version_number,
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
            optional_extensions,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Err(e) => {
            eprintln!("Couldn't open {}: {}", path.display(), e);
            return;
        }
        Ok(file) => file,
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Err(e) => {
            eprintln!("Couldn't read {}: {}", path.display(), e);
            return;
        }
        Ok(_) => {}
    };

    match parse_ntp_packet(&buffer) {
        Ok((_, packet)) => println!("{:?}", packet),
        Err(e) => eprintln!("Failed to parse NTP packet: {:?}", e),
    }
}