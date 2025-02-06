use nom::{
    bytes::complete::tag,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
struct JpegMarker {
    marker: u8,
    length: u16,
    data: Vec<u8>,
}

fn jpeg_marker(input: &[u8]) -> IResult<&[u8], JpegMarker> {
    let (input, _) = tag(&[0xFF])(input)?;
    let (input, marker_code) = be_u8(input)?;
    let (input, length) = be_u16(input)?;
    let (input, data) = nom::bytes::complete::take((length - 2) as usize)(input)?;
    Ok((
        input,
        JpegMarker {
            marker: marker_code,
            length: length,
            data: data.to_vec(),
        },
    ))
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], Vec<JpegMarker>> {
    let mut markers = Vec::new();
    let mut remaining_input = input;
    loop {
        match jpeg_marker(remaining_input) {
            Ok((rest, marker)) => {
                markers.push(marker.clone());
                remaining_input = rest;
                if marker.marker == 0xD9 {
                    break;
                }
            }
            Err(_) => break,
        }
    }
    Ok((remaining_input, markers))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <JPEG_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match parse_jpeg(&buffer) {
        Ok((_, markers)) => {
            println!("JPEG Markers:");
            for marker in markers {
                println!("{:?}", marker);
            }
        }
        Err(e) => {
            println!("Error parsing JPEG: {:?}", e);
        }
    }
}
