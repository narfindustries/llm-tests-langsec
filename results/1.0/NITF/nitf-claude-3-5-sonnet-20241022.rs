use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NITF {
    file_header: FileHeader,
    image_segments: Vec<ImageSegment>,
}

#[derive(Debug)]
struct FileHeader {
    file_profile_name: String,
    file_version: String,
    complexity_level: u8,
    standard_type: String,
    originating_station: String,
    file_date_time: String,
    file_title: String,
    security_classification: String,
    encryption: String,
    file_copy_number: String,
    file_number_of_copies: String,
    background_color: String,
    originator_name: String,
    originator_phone: String,
    file_length: u32,
    header_length: u32,
    num_image_segments: u16,
}

#[derive(Debug)]
struct ImageSegment {
    im: String,
    iid1: String,
    idatim: String,
    tgtid: String,
    iid2: String,
    security_classification: String,
    encryption: String,
    image_source: String,
    num_significant_rows: u32,
    num_significant_cols: u32,
    pixel_value_type: String,
    image_representation: String,
    image_category: String,
    actual_bits_per_pixel: u8,
    pixel_justification: String,
    image_coord_system: String,
    image_comments: Vec<String>,
    compression: String,
    compression_rate_code: String,
    num_bands: u8,
    image_sync_code: u8,
}

fn parse_fixed_length_string(input: &[u8], length: usize) -> IResult<&[u8], String> {
    let (remaining, data) = take(length)(input)?;
    Ok((
        remaining,
        String::from_utf8_lossy(data).trim().to_string(),
    ))
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, _) = tag(b"NITF")(input)?;
    let (input, file_profile_name) = parse_fixed_length_string(input, 4)?;
    let (input, file_version) = parse_fixed_length_string(input, 5)?;
    let (input, complexity_level) = be_u8(input)?;
    let (input, standard_type) = parse_fixed_length_string(input, 2)?;
    let (input, originating_station) = parse_fixed_length_string(input, 10)?;
    let (input, file_date_time) = parse_fixed_length_string(input, 14)?;
    let (input, file_title) = parse_fixed_length_string(input, 80)?;
    let (input, security_classification) = parse_fixed_length_string(input, 1)?;
    let (input, encryption) = parse_fixed_length_string(input, 1)?;
    let (input, file_copy_number) = parse_fixed_length_string(input, 5)?;
    let (input, file_number_of_copies) = parse_fixed_length_string(input, 5)?;
    let (input, background_color) = parse_fixed_length_string(input, 3)?;
    let (input, originator_name) = parse_fixed_length_string(input, 24)?;
    let (input, originator_phone) = parse_fixed_length_string(input, 18)?;
    let (input, file_length) = be_u32(input)?;
    let (input, header_length) = be_u32(input)?;
    let (input, num_image_segments) = be_u16(input)?;

    Ok((
        input,
        FileHeader {
            file_profile_name,
            file_version,
            complexity_level,
            standard_type,
            originating_station,
            file_date_time,
            file_title,
            security_classification,
            encryption,
            file_copy_number,
            file_number_of_copies,
            background_color,
            originator_name,
            originator_phone,
            file_length,
            header_length,
            num_image_segments,
        },
    ))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, im) = parse_fixed_length_string(input, 2)?;
    let (input, iid1) = parse_fixed_length_string(input, 10)?;
    let (input, idatim) = parse_fixed_length_string(input, 14)?;
    let (input, tgtid) = parse_fixed_length_string(input, 17)?;
    let (input, iid2) = parse_fixed_length_string(input, 80)?;
    let (input, security_classification) = parse_fixed_length_string(input, 1)?;
    let (input, encryption) = parse_fixed_length_string(input, 1)?;
    let (input, image_source) = parse_fixed_length_string(input, 42)?;
    let (input, num_significant_rows) = be_u32(input)?;
    let (input, num_significant_cols) = be_u32(input)?;
    let (input, pixel_value_type) = parse_fixed_length_string(input, 3)?;
    let (input, image_representation) = parse_fixed_length_string(input, 8)?;
    let (input, image_category) = parse_fixed_length_string(input, 8)?;
    let (input, actual_bits_per_pixel) = be_u8(input)?;
    let (input, pixel_justification) = parse_fixed_length_string(input, 1)?;
    let (input, image_coord_system) = parse_fixed_length_string(input, 1)?;
    
    let mut image_comments = Vec::new();
    let (input, num_comments) = be_u8(input)?;
    let mut current_input = input;
    for _ in 0..num_comments {
        let (new_input, comment) = parse_fixed_length_string(current_input, 80)?;
        image_comments.push(comment);
        current_input = new_input;
    }
    
    let (input, compression) = parse_fixed_length_string(current_input, 2)?;
    let (input, compression_rate_code) = parse_fixed_length_string(input, 4)?;
    let (input, num_bands) = be_u8(input)?;
    let (input, image_sync_code) = be_u8(input)?;

    Ok((
        input,
        ImageSegment {
            im,
            iid1,
            idatim,
            tgtid,
            iid2,
            security_classification,
            encryption,
            image_source,
            num_significant_rows,
            num_significant_cols,
            pixel_value_type,
            image_representation,
            image_category,
            actual_bits_per_pixel,
            pixel_justification,
            image_coord_system,
            image_comments,
            compression,
            compression_rate_code,
            num_bands,
            image_sync_code,
        },
    ))
}

fn parse_nitf(input: &[u8]) -> IResult<&[u8], NITF> {
    let (input, file_header) = parse_file_header(input)?;
    let mut image_segments = Vec::new();
    let mut current_input = input;

    for _ in 0..file_header.num_image_segments {
        let (new_input, image_segment) = parse_image_segment(current_input)?;
        image_segments.push(image_segment);
        current_input = new_input;
    }

    Ok((
        current_input,
        NITF {
            file_header,
            image_segments,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_nitf(&buffer) {
        Ok((_, nitf)) => println!("{:#?}", nitf),
        Err(e) => eprintln!("Failed to parse NITF file: {:?}", e),
    }
}