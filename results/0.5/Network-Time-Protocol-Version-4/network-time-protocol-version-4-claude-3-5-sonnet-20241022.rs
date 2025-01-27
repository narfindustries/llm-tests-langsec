use nom::{
    bits::complete::take,
    combinator::map,
    error::Error,
    number::complete::{be_f32, be_i32, be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

#[derive(Debug)]
struct NTPTimestamp {
    seconds: u32,
    fraction: u32,
}

#[derive(Debug)]
struct ReferenceIdentifier {
    id: [u8; 4],
}

#[derive(Debug)]
struct NTPPacket {
    li_vn_mode: u8,
    stratum: u8,
    poll: i8,
    precision: i8,
    root_delay: f32,
    root_dispersion: f32,
    reference_id: ReferenceIdentifier,
    reference_timestamp: NTPTimestamp,
    origin_timestamp: NTPTimestamp,
    receive_timestamp: NTPTimestamp,
    transmit_timestamp: NTPTimestamp,
    key_identifier: Option<u32>,
    message_digest: Option<[u8; 16]>,
}

fn parse_timestamp(input: &[u8]) -> IResult<&[u8], NTPTimestamp> {
    let (input, (seconds, fraction)) = tuple((be_u32, be_u32))(input)?;
    Ok((
        input,
        NTPTimestamp {
            seconds,
            fraction,
        },
    ))
}

fn parse_reference_id(input: &[u8]) -> IResult<&[u8], ReferenceIdentifier> {
    let (input, id) = map(take(4usize), |bytes: &[u8]| {
        let mut arr = [0u8; 4];
        arr.copy_from_slice(bytes);
        arr
    })(input)?;
    Ok((input, ReferenceIdentifier { id }))
}

fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NTPPacket> {
    let (input, (
        li_vn_mode,
        stratum,
        poll,
        precision,
        root_delay,
        root_dispersion,
        reference_id,
        reference_timestamp,
        origin_timestamp,
        receive_timestamp,
        transmit_timestamp,
    )) = tuple((
        be_u8,
        be_u8,
        be_i32,
        be_i32,
        be_f32,
        be_f32,
        parse_reference_id,
        parse_timestamp,
        parse_timestamp,
        parse_timestamp,
        parse_timestamp,
    ))(input)?;

    let (input, key_identifier) = if input.len() >= 4 {
        let (input, key_id) = be_u32(input)?;
        (input, Some(key_id))
    } else {
        (input, None)
    };

    let (input, message_digest) = if input.len() >= 16 {
        let (input, digest) = map(take(16usize), |bytes: &[u8]| {
            let mut arr = [0u8; 16];
            arr.copy_from_slice(bytes);
            arr
        })(input)?;
        (input, Some(digest))
    } else {
        (input, None)
    };

    Ok((
        input,
        NTPPacket {
            li_vn_mode,
            stratum,
            poll: poll as i8,
            precision: precision as i8,
            root_delay,
            root_dispersion,
            reference_id,
            reference_timestamp,
            origin_timestamp,
            receive_timestamp,
            transmit_timestamp,
            key_identifier,
            message_digest,
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <ntp_binary_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_ntp_packet(&buffer) {
        Ok((remaining, packet)) => {
            println!("Parsed NTP packet: {:#?}", packet);
            if !remaining.is_empty() {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
        }
        Err(e) => eprintln!("Failed to parse NTP packet: {:?}", e),
    }

    Ok(())
}