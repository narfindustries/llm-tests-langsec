use nom::{
    IResult,
    bytes::complete::take,
    number::complete::le_u32,
    multi::many0,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NitfFile {
    header: NitfHeader,
    image_segments: Vec<ImageSegment>,
}

#[derive(Debug)]
struct NitfHeader {
    fhdr: String,
    clevel: u8,
    stype: String,
    ostaid: String,
    fdt: String,
}

#[derive(Debug)]
struct ImageSegment {
    image_header: ImageHeader,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct ImageHeader {
    icat: String,
    iclod: String,
    encryp: u8,
    icords: String,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, fhdr) = take(9usize)(input)?;
    let (input, clevel) = take(1usize)(input)?;
    let (input, stype) = take(1usize)(input)?;
    let (input, ostaid) = take(10usize)(input)?;
    let (input, fdt) = take(14usize)(input)?;

    Ok((input, NitfHeader {
        fhdr: String::from_utf8_lossy(fhdr).to_string(),
        clevel: clevel[0],
        stype: String::from_utf8_lossy(stype).to_string(),
        ostaid: String::from_utf8_lossy(ostaid).to_string(),
        fdt: String::from_utf8_lossy(fdt).to_string(),
    }))
}

fn parse_image_header(input: &[u8]) -> IResult<&[u8], ImageHeader> {
    let (input, icat) = take(2usize)(input)?;
    let (input, iclod) = take(1usize)(input)?;
    let (input, encryp) = take(1usize)(input)?;
    let (input, icords) = take(1usize)(input)?;

    Ok((input, ImageHeader {
        icat: String::from_utf8_lossy(icat).to_string(),
        iclod: String::from_utf8_lossy(iclod).to_string(),
        encryp: encryp[0],
        icords: String::from_utf8_lossy(icords).to_string(),
    }))
}

fn parse_nitf(input: &[u8]) -> IResult<&[u8], NitfFile> {
    let (input, header) = parse_nitf_header(input)?;
    let (input, image_segments) = many0(parse_image_segment)(input)?;

    Ok((input, NitfFile {
        header,
        image_segments,
    }))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, image_header) = parse_image_header(input)?;
    let (input, image_data_size) = le_u32(input)?;
    let (input, image_data) = take(image_data_size as usize)(input)?;

    Ok((input, ImageSegment {
        image_header,
        image_data: image_data.to_vec(),
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf(&buffer) {
        Ok((_, nitf)) => {
            println!("Parsed NITF: {:?}", nitf);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            Err(Box::new(std::io::Error::new(std::io::ErrorKind::InvalidData, format!("{:?}", e))))
        }
    }
}