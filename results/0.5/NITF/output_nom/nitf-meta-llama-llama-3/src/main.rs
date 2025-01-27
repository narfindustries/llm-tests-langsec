use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    character::complete::{digit1, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    IResult,
};
use std::str;

// Define NITF file format constants
const NITF_FILE_HEADER_SIZE: usize = 12;
const NITF_FILE_HEADER_SIGNATURE: &[u8] = b"NITF01.1";
const NITF_FILE_TRAILER_SIZE: usize = 18;
const NITF_FILE_TRAILER_SIGNATURE: &[u8] = b"NTFR";

// Define NITF file header structure
#[derive(Debug)]
struct NITFFileHeader {
    signature: String,
    file_format_version: String,
    file_style: u8,
    file_security_classification: String,
}

// Define NITF file trailer structure
#[derive(Debug)]
struct NITFFileTrailer {
    signature: String,
    file_length: u32,
    file_id: String,
}

// Define NITF image segment structure
#[derive(Debug)]
struct NITFImageSegment {
    segment_type: String,
    image_id: String,
    image_format: String,
    image_data: Vec<u8>,
}

// Define NITF file structure
#[derive(Debug)]
struct NITFFile {
    file_header: NITFFileHeader,
    image_segments: Vec<NITFImageSegment>,
    file_trailer: NITFFileTrailer,
}

// Define NITF file parser
fn parse_nitf_file(input: &[u8]) -> IResult<&[u8], NITFFile> {
    let (input, file_header) = parse_nitf_file_header(input)?;
    let (input, image_segments) = many0(parse_nitf_image_segment)(input)?;
    let (input, file_trailer) = parse_nitf_file_trailer(input)?;
    Ok((input, NITFFile {
        file_header,
        image_segments,
        file_trailer,
    }))
}

// Define NITF file header parser
fn parse_nitf_file_header(input: &[u8]) -> IResult<&[u8], NITFFileHeader> {
    let (input, _) = tag(NITF_FILE_HEADER_SIGNATURE)(input)?;
    let (input, _file_format_version) = take(5usize)(input)?;
    let (input, file_format_version) = map_res(_file_format_version, str::from_utf8)(input)?;
    let (input, _file_style) = take(1usize)(input)?;
    let (input, file_style) = map(_file_style, |x| x[0])(input)?;
    let (input, _file_security_classification) = take(3usize)(input)?;
    let (input, file_security_classification) = map_res(_file_security_classification, str::from_utf8)(input)?;
    Ok((
        input,
        NITFFileHeader {
            signature: String::from_utf8_lossy(NITF_FILE_HEADER_SIGNATURE).into_owned(),
            file_format_version: file_format_version.to_string(),
            file_style,
            file_security_classification: file_security_classification.to_string(),
        },
    ))
}

// Define NITF image segment parser
fn parse_nitf_image_segment(input: &[u8]) -> IResult<&[u8], NITFImageSegment> {
    let (input, segment_type) = take(2usize)(input)?;
    let (input, image_id) = take(25usize)(input)?;
    let (input, image_format) = take(2usize)(input)?;
    let (input, image_data_length) = map_res(take(5usize)(input)?, |x| std::str::from_utf8(x).unwrap().parse::<u32>())?;
    let (input, image_data) = take(image_data_length as usize)(input)?;
    Ok((
        input,
        NITFImageSegment {
            segment_type: String::from_utf8_lossy(segment_type).into_owned(),
            image_id: String::from_utf8_lossy(image_id).into_owned(),
            image_format: String::from_utf8_lossy(image_format).into_owned(),
            image_data: image_data.to_vec(),
        },
    ))
}

// Define NITF file trailer parser
fn parse_nitf_file_trailer(input: &[u8]) -> IResult<&[u8], NITFFileTrailer> {
    let (input, _) = tag(NITF_FILE_TRAILER_SIGNATURE)(input)?;
    let (input, _file_length) = take(6usize)(input)?;
    let (input, file_length) = map_res(_file_length, |x| std::str::from_utf8(x).unwrap().parse::<u32>())?;
    let (input, _file_id) = take(25usize)(input)?;
    let (input, file_id) = map_res(_file_id, str::from_utf8)(input)?;
    Ok((
        input,
        NITFFileTrailer {
            signature: String::from_utf8_lossy(NITF_FILE_TRAILER_SIGNATURE).into_owned(),
            file_length,
            file_id: file_id.to_string(),
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).unwrap();
    let mut input = Vec::new();
    file.read_to_end(&mut input).unwrap();
    match parse_nitf_file(&input) {
        Ok((_, nitf_file)) => println!("{:?}", nitf_file),
        Err(err) => println!("Error parsing NITF file: {}", err),
    }
}