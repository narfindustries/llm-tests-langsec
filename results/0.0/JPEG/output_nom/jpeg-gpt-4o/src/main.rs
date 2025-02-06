use nom::{
    bytes::complete::{tag, take},
    multi::many0,
    number::complete::be_u16,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
enum Marker {
    SOI,
    APPn(u8),
    DQT,
    SOF(u8),
    DHT,
    SOS,
    EOI,
    DRI,
    RSTn(u8),
    COM,
    Unknown(u8),
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], Marker> {
    let (input, marker) = be_u16(input)?;
    let marker_type = match marker {
        0xFFD8 => Marker::SOI,
        0xFFE0..=0xFFEF => Marker::APPn((marker & 0xFF) as u8),
        0xFFDB => Marker::DQT,
        0xFFC0..=0xFFC3 => Marker::SOF((marker & 0xF) as u8),
        0xFFC4 => Marker::DHT,
        0xFFDA => Marker::SOS,
        0xFFD9 => Marker::EOI,
        0xFFDD => Marker::DRI,
        0xFFD0..=0xFFD7 => Marker::RSTn((marker & 0xF) as u8),
        0xFFFE => Marker::COM,
        _ => Marker::Unknown((marker & 0xFF) as u8),
    };
    Ok((input, marker_type))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, length) = be_u16(input)?;
    let (input, data) = take(length - 2)(input)?;
    Ok((input, data.to_vec()))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<(Marker, Vec<u8>)>> {
    let (input, _) = tag([0xFF, 0xD8])(input)?; // SOI
    many0(|input| {
        let (input, marker) = parse_marker(input)?;
        let (input, data) = if let Marker::EOI = marker {
            (input, vec![])
        } else {
            parse_segment(input)?
        };
        Ok((input, (marker, data)))
    })(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_jpeg(&buffer) {
        Ok((_, segments)) => {
            for (marker, data) in segments {
                println!("{:?}: {} bytes", marker, data.len());
            }
        }
        Err(e) => eprintln!("Failed to parse JPEG: {:?}", e),
    }
}