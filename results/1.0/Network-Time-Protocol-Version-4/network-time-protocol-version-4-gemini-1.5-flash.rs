use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::read;
use std::net::Ipv4Addr;

#[derive(Debug)]
struct NtpPacket {
    li: u8,
    vn: u8,
    mode: u8,
    stratum: u8,
    poll: u8,
    precision: i8,
    root_delay: f64,
    root_dispersion: f64,
    reference_id: String,
    reference_timestamp: u64,
    originate_timestamp: u64,
    receive_timestamp: u64,
    transmit_timestamp: u64,
    // Add extension fields here if needed (RFC 5905 details optional fields)
}


fn parse_ntp_packet(input: &[u8]) -> IResult<&[u8], NtpPacket> {
    let (rest, li_vn_mode) = be_u16(input)?;
    let li = (li_vn_mode >> 6) as u8;
    let vn = ((li_vn_mode >> 3) & 0x7) as u8;
    let mode = (li_vn_mode & 0x7) as u8;

    let (rest, stratum) = be_u8(rest)?;
    let (rest, poll) = be_u8(rest)?;
    let (rest, precision) = map_res(be_u8, |x| {
        if x > 127 {
            Ok(-((x as i16) - 255) as i8)
        } else {
            Ok(x as i8)
        }
    })(rest)?;
    let (rest, root_delay) = map_res(be_u32, |x| {
        let delay = x as f64 / (1 << 16);
        Ok(delay)
    })(rest)?;
    let (rest, root_dispersion) = map_res(be_u32, |x| {
        let dispersion = x as f64 / (1 << 16);
        Ok(dispersion)
    })(rest)?;

    let (rest, reference_id_bytes) = take(4usize)(rest)?;
    let reference_id = match Ipv4Addr::new(reference_id_bytes[0], reference_id_bytes[1], reference_id_bytes[2], reference_id_bytes[3]) {
        Ok(ip) => ip.to_string(),
        Err(_) => String::from_utf8_lossy(reference_id_bytes).to_string(),
    };


    let (rest, reference_timestamp) = map(take(8usize), |x| u64::from_be_bytes(x.try_into().unwrap()))(rest)?;
    let (rest, originate_timestamp) = map(take(8usize), |x| u64::from_be_bytes(x.try_into().unwrap()))(rest)?;
    let (rest, receive_timestamp) = map(take(8usize), |x| u64::from_be_bytes(x.try_into().unwrap()))(rest)?;
    let (rest, transmit_timestamp) = map(take(8usize), |x| u64::from_be_bytes(x.try_into().unwrap()))(rest)?;

    Ok((
        rest,
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <binary_file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let data = read(filename)?;

    match parse_ntp_packet(&data) {
        Ok((_, packet)) => println!("{:#?}", packet),
        Err(e) => eprintln!("Error parsing NTP packet: {:?}", e),
    }

    Ok(())
}
