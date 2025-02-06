use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NitfHeader {
    fhdr: String,
    clevel: String,
    stype: String,
    ostaid: Option<String>,
    fl: u64,
    encryp: u8,
    image_segments: Vec<ImageSegment>,
}

#[derive(Debug)]
struct ImageSegment {
    im: String,
    isorce: Option<String>,
    nrows: u32,
    ncols: u32,
    pvtype: String,
    imode: String,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, (
        fhdr,
        clevel,
        stype,
        ostaid,
        fl,
        encryp,
        image_segments
    )) = tuple((
        map(take(9usize), |b: &[u8]| String::from_utf8_lossy(b).to_string()),
        map(take(2usize), |b: &[u8]| String::from_utf8_lossy(b).to_string()),
        map(take(1usize), |b: &[u8]| String::from_utf8_lossy(b).to_string()),
        opt(map(take(10usize), |b: &[u8]| String::from_utf8_lossy(b).to_string())),
        map(take(3usize), |b: &[u8]| String::from_utf8_lossy(b).parse::<u64>().unwrap()),
        map(take(1usize), |b: &[u8]| b[0] - b'0'),
        many0(parse_image_segment)
    ))(input)?;

    Ok((input, NitfHeader {
        fhdr,
        clevel,
        stype,
        ostaid,
        fl,
        encryp,
        image_segments,
    }))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, (
        im,
        isorce,
        nrows,
        ncols,
        pvtype,
        imode
    )) = tuple((
        map(take(10usize), |b: &[u8]| String::from_utf8_lossy(b).to_string()),
        opt(map(take(42usize), |b: &[u8]| String::from_utf8_lossy(b).to_string())),
        map(take(8usize), |b: &[u8]| String::from_utf8_lossy(b).parse::<u32>().unwrap()),
        map(take(8usize), |b: &[u8]| String::from_utf8_lossy(b).parse::<u32>().unwrap()),
        map(take(3usize), |b: &[u8]| String::from_utf8_lossy(b).to_string()),
        map(take(1usize), |b: &[u8]| String::from_utf8_lossy(b).to_string())
    ))(input)?;

    Ok((input, ImageSegment {
        im,
        isorce,
        nrows,
        ncols,
        pvtype,
        imode,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf_header(&buffer) {
        Ok((_, nitf)) => {
            println!("NITF Header: {:?}", nitf);
            Ok(())
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}