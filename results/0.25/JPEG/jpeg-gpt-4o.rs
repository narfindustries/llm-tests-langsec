use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::many0,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
enum JPEGMarker {
    SOI,
    APPn(u8),
    COM,
    DQT,
    SOF(u8),
    DHT,
    SOS,
    EOI,
    RST(u8),
    Unknown(u8),
}

#[derive(Debug)]
struct JPEGSegment {
    marker: JPEGMarker,
    data: Vec<u8>,
}

fn parse_marker(input: &[u8]) -> IResult<&[u8], JPEGMarker> {
    let (input, marker) = be_u16(input)?;
    let marker_type = match marker {
        0xFFD8 => JPEGMarker::SOI,
        0xFFE0..=0xFFEF => JPEGMarker::APPn((marker & 0xFF) as u8),
        0xFFFE => JPEGMarker::COM,
        0xFFDB => JPEGMarker::DQT,
        0xFFC0..=0xFFCF => JPEGMarker::SOF((marker & 0xF) as u8),
        0xFFC4 => JPEGMarker::DHT,
        0xFFDA => JPEGMarker::SOS,
        0xFFD9 => JPEGMarker::EOI,
        0xFFD0..=0xFFD7 => JPEGMarker::RST((marker & 0xF) as u8),
        _ => JPEGMarker::Unknown((marker & 0xFF) as u8),
    };
    Ok((input, marker_type))
}

fn parse_segment(input: &[u8]) -> IResult<&[u8], JPEGSegment> {
    let (input, marker) = parse_marker(input)?;
    let (input, length) = match marker {
        JPEGMarker::SOI | JPEGMarker::EOI => (input, 0),
        _ => {
            let (input, len) = be_u16(input)?;
            (input, len as usize - 2)
        }
    };
    let (input, data) = take(length)(input)?;
    Ok((
        input,
        JPEGSegment {
            marker,
            data: data.to_vec(),
        },
    ))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<JPEGSegment>> {
    many0(parse_segment)(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_jpeg(&buffer) {
        Ok((_, segments)) => {
            for segment in segments {
                println!("{:?}", segment);
            }
        }
        Err(e) => eprintln!("Error parsing JPEG: {:?}", e),
    }

    Ok(())
}