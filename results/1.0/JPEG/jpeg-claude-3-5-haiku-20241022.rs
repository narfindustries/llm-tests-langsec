use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0, many1},
    number::complete::{be_u8, be_u16},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct App {
    soi: u16,
    app_marker: u16,
    length: u16,
    identifier: Vec<u8>,
    version: Option<(u8, u8)>,
    density_units: Option<u8>,
    x_density: Option<u16>,
    y_density: Option<u16>,
    thumbnail: Option<Vec<u8>>,
}

#[derive(Debug)]
struct JpegFile {
    start_of_image: u16,
    app_segments: Vec<App>,
}

fn parse_app_segment(input: &[u8]) -> IResult<&[u8], App> {
    map(
        tuple((
            be_u16,
            be_u16,
            be_u16,
            many1(be_u8),
            opt(pair(be_u8, be_u8)),
            opt(be_u8),
            opt(be_u16),
            opt(be_u16),
            opt(many0(be_u8)),
        )),
        |(soi, app_marker, length, identifier, version, density_units, x_density, y_density, thumbnail)| App {
            soi,
            app_marker,
            length,
            identifier,
            version,
            density_units,
            x_density,
            y_density,
            thumbnail,
        },
    )(input)
}

fn parse_jpeg(input: &[u8]) -> IResult<&[u8], JpegFile> {
    map(
        tuple((
            tag(&[0xFF, 0xD8]),
            many0(parse_app_segment),
        )),
        |(start_of_image, app_segments)| JpegFile {
            start_of_image: u16::from_be_bytes([start_of_image[0], start_of_image[1]]),
            app_segments,
        },
    )(input)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <jpeg_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_jpeg(&buffer) {
        Ok((_, jpeg)) => {
            println!("Parsed JPEG: {:?}", jpeg);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing JPEG: {:?}", e);
            std::process::exit(1);
        }
    }
}