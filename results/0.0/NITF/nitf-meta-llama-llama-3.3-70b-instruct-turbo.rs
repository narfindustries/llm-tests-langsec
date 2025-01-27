use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{tuple, preceded},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
struct NITFHeader {
    file_header: String,
    file_version: u8,
    file_level: u8,
    system_name: String,
    file_title: String,
    file_security_classification: String,
    file_control_number: String,
    file_date: String,
}

#[derive(Debug)]
struct NITFImageSegmentHeader {
    segment_type: String,
    segment_length: u32,
    image_id: String,
    image_date: String,
    image_security_classification: String,
    image_control_number: String,
    image_representation: u8,
    image_ig_name: String,
    image_ig_version: u8,
    num_image_bands: u16,
    num_image_rows: u32,
    num_image_columns: u32,
    pixel_value_type: u8,
    pixel_justification: u8,
    image_compression: u8,
    num_blocks_per_row: u32,
    num_blocks_per_column: u32,
    block_size_rows: u32,
    block_size_columns: u32,
}

#[derive(Debug)]
struct NITFImageSegment {
    header: NITFImageSegmentHeader,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct NITFFile {
    header: NITFHeader,
    image_segments: Vec<NITFImageSegment>,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, file_header) = take(4u8)(input)?;
    let (input, file_version) = be_u8(input)?;
    let (input, file_level) = be_u8(input)?;
    let (input, system_name) = take(25u8)(input)?;
    let (input, file_title) = take(80u8)(input)?;
    let (input, file_security_classification) = take(1u8)(input)?;
    let (input, file_control_number) = take(25u8)(input)?;
    let (input, file_date) = take(10u8)(input)?;
    Ok((
        input,
        NITFHeader {
            file_header: String::from_utf8_lossy(file_header).into_owned(),
            file_version,
            file_level,
            system_name: String::from_utf8_lossy(system_name).into_owned(),
            file_title: String::from_utf8_lossy(file_title).into_owned(),
            file_security_classification: String::from_utf8_lossy(file_security_classification)
                .into_owned(),
            file_control_number: String::from_utf8_lossy(file_control_number).into_owned(),
            file_date: String::from_utf8_lossy(file_date).into_owned(),
        },
    ))
}

fn parse_nITF_image_segment_header(input: &[u8]) -> IResult<&[u8], NITFImageSegmentHeader> {
    let (input, segment_type) = take(2u8)(input)?;
    let (input, segment_length) = be_u32(input)?;
    let (input, image_id) = take(25u8)(input)?;
    let (input, image_date) = take(10u8)(input)?;
    let (input, image_security_classification) = take(1u8)(input)?;
    let (input, image_control_number) = take(25u8)(input)?;
    let (input, image_representation) = be_u8(input)?;
    let (input, image_ig_name) = take(25u8)(input)?;
    let (input, image_ig_version) = be_u8(input)?;
    let (input, num_image_bands) = be_u16(input)?;
    let (input, num_image_rows) = be_u32(input)?;
    let (input, num_image_columns) = be_u32(input)?;
    let (input, pixel_value_type) = be_u8(input)?;
    let (input, pixel_justification) = be_u8(input)?;
    let (input, image_compression) = be_u8(input)?;
    let (input, num_blocks_per_row) = be_u32(input)?;
    let (input, num_blocks_per_column) = be_u32(input)?;
    let (input, block_size_rows) = be_u32(input)?;
    let (input, block_size_columns) = be_u32(input)?;
    Ok((
        input,
        NITFImageSegmentHeader {
            segment_type: String::from_utf8_lossy(segment_type).into_owned(),
            segment_length,
            image_id: String::from_utf8_lossy(image_id).into_owned(),
            image_date: String::from_utf8_lossy(image_date).into_owned(),
            image_security_classification: String::from_utf8_lossy(image_security_classification)
                .into_owned(),
            image_control_number: String::from_utf8_lossy(image_control_number).into_owned(),
            image_representation,
            image_ig_name: String::from_utf8_lossy(image_ig_name).into_owned(),
            image_ig_version,
            num_image_bands,
            num_image_rows,
            num_image_columns,
            pixel_value_type,
            pixel_justification,
            image_compression,
            num_blocks_per_row,
            num_blocks_per_column,
            block_size_rows,
            block_size_columns,
        },
    ))
}

fn parse_nitf_image_segment(input: &[u8]) -> IResult<&[u8], NITFImageSegment> {
    let (input, header) = parse_nITF_image_segment_header(input)?;
    let (input, image_data) = length_data(header.segment_length)(input)?;
    Ok((input, NITFImageSegment { header, image_data: image_data.to_vec() }))
}

fn parse_nitf_file(input: &[u8]) -> IResult<&[u8], NITFFile> {
    let (input, header) = parse_nitf_header(input)?;
    let (input, image_segments) = many0(parse_nitf_image_segment)(input)?;
    Ok((input, NITFFile { header, image_segments }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    match parse_nitf_file(&input) {
        Ok((_, nitf_file)) => println!("{:?}", nitf_file),
        Err(err) => println!("Error parsing NITF file: {:?}", err),
    }
}