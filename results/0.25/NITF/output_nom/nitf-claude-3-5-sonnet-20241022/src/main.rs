use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NITF {
    header: NITFHeader,
    image_segments: Vec<ImageSegment>,
}

#[derive(Debug)]
struct NITFHeader {
    file_profile_name: [u8; 4],
    file_version: [u8; 5],
    complexity_level: [u8; 2],
    standard_type: [u8; 4],
    originating_station_id: [u8; 10],
    file_date_time: [u8; 14],
    file_title: [u8; 80],
    file_security_classification: u8,
    encryption: [u8; 1],
    message_copy: [u8; 1],
    message_handling: [u8; 2],
    precedence: [u8; 1],
    message_id: [u8; 14],
    message_type: [u8; 2],
    originator_name: [u8; 24],
    recipient_name: Vec<[u8; 24]>,
    file_length: u32,
    header_length: u32,
    num_image_segments: u16,
    num_graphic_segments: u16,
    reserved_segment_info: [u8; 3],
}

#[derive(Debug)]
struct ImageSegment {
    im: [u8; 2],
    image_identifier: [u8; 10],
    image_date_time: [u8; 14],
    target_identifier: [u8; 17],
    image_identifier2: [u8; 80],
    security_classification: u8,
    encryption: [u8; 1],
    image_source: [u8; 42],
    num_significant_rows: u32,
    num_significant_cols: u32,
    pixel_value_type: [u8; 3],
    image_representation: [u8; 8],
    image_category: [u8; 8],
    actual_bits_per_pixel: u8,
    pixel_justification: [u8; 1],
    image_coordinate_system: [u8; 1],
    image_geo_location: [u8; 60],
    num_image_comments: u16,
    image_comments: Vec<[u8; 80]>,
    image_compression: [u8; 2],
    compression_rate_code: [u8; 4],
    num_bands: u16,
    image_sync_code: u8,
    image_mode: [u8; 1],
    num_blocks_per_row: u16,
    num_blocks_per_col: u16,
    num_pixels_per_block_horz: u16,
    num_pixels_per_block_vert: u16,
    num_pixels_per_band: u16,
    image_display_level: u16,
    attachment_level: u16,
    image_location: u32,
    image_magnification: [u8; 4],
    user_defined_image_data_length: u32,
    user_defined_overflow: u32,
    image_data_length: u32,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, (
        file_profile_name,
        file_version,
        complexity_level,
        standard_type,
        originating_station_id,
        file_date_time,
        file_title,
        file_security_classification,
        encryption,
        message_copy,
        message_handling,
        precedence,
        message_id,
        message_type,
        originator_name,
        num_recipients,
    )) = tuple((
        take(4usize),
        take(5usize),
        take(2usize),
        take(4usize),
        take(10usize),
        take(14usize),
        take(80usize),
        be_u8,
        take(1usize),
        take(1usize),
        take(2usize),
        take(1usize),
        take(14usize),
        take(2usize),
        take(24usize),
        be_u8,
    ))(input)?;

    let (input, recipient_name) = count(take(24usize), num_recipients as usize)(input)?;

    let (input, (
        file_length,
        header_length,
        num_image_segments,
        num_graphic_segments,
        reserved_segment_info,
    )) = tuple((
        be_u32,
        be_u32,
        be_u16,
        be_u16,
        take(3usize),
    ))(input)?;

    Ok((input, NITFHeader {
        file_profile_name: file_profile_name.try_into().unwrap(),
        file_version: file_version.try_into().unwrap(),
        complexity_level: complexity_level.try_into().unwrap(),
        standard_type: standard_type.try_into().unwrap(),
        originating_station_id: originating_station_id.try_into().unwrap(),
        file_date_time: file_date_time.try_into().unwrap(),
        file_title: file_title.try_into().unwrap(),
        file_security_classification,
        encryption: encryption.try_into().unwrap(),
        message_copy: message_copy.try_into().unwrap(),
        message_handling: message_handling.try_into().unwrap(),
        precedence: precedence.try_into().unwrap(),
        message_id: message_id.try_into().unwrap(),
        message_type: message_type.try_into().unwrap(),
        originator_name: originator_name.try_into().unwrap(),
        recipient_name: recipient_name.into_iter().map(|x| x.try_into().unwrap()).collect(),
        file_length,
        header_length,
        num_image_segments,
        num_graphic_segments,
        reserved_segment_info: reserved_segment_info.try_into().unwrap(),
    }))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, (
        im,
        image_identifier,
        image_date_time,
        target_identifier,
        image_identifier2,
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
        image_coordinate_system,
        image_geo_location,
        num_image_comments,
    )) = tuple((
        take(2usize),
        take(10usize),
        take(14usize),
        take(17usize),
        take(80usize),
        be_u8,
        take(1usize),
        take(42usize),
        be_u32,
        be_u32,
        take(3usize),
        take(8usize),
        take(8usize),
        be_u8,
        take(1usize),
        take(1usize),
        take(60usize),
        be_u16,
    ))(input)?;

    let (input, image_comments) = count(take(80usize), num_image_comments as usize)(input)?;

    let (input, (
        image_compression,
        compression_rate_code,
        num_bands,
        image_sync_code,
        image_mode,
        num_blocks_per_row,
        num_blocks_per_col,
        num_pixels_per_block_horz,
        num_pixels_per_block_vert,
        num_pixels_per_band,
        image_display_level,
        attachment_level,
        image_location,
        image_magnification,
        user_defined_image_data_length,
        user_defined_overflow,
        image_data_length,
    )) = tuple((
        take(2usize),
        take(4usize),
        be_u16,
        be_u8,
        take(1usize),
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u32,
        take(4usize),
        be_u32,
        be_u32,
        be_u32,
    ))(input)?;

    Ok((input, ImageSegment {
        im: im.try_into().unwrap(),
        image_identifier: image_identifier.try_into().unwrap(),
        image_date_time: image_date_time.try_into().unwrap(),
        target_identifier: target_identifier.try_into().unwrap(),
        image_identifier2: image_identifier2.try_into().unwrap(),
        security_classification,
        encryption: encryption.try_into().unwrap(),
        image_source: image_source.try_into().unwrap(),
        num_significant_rows,
        num_significant_cols,
        pixel_value_type: pixel_value_type.try_into().unwrap(),
        image_representation: image_representation.try_into().unwrap(),
        image_category: image_category.try_into().unwrap(),
        actual_bits_per_pixel,
        pixel_justification: pixel_justification.try_into().unwrap(),
        image_coordinate_system: image_coordinate_system.try_into().unwrap(),
        image_geo_location: image_geo_location.try_into().unwrap(),
        num_image_comments,
        image_comments: image_comments.into_iter().map(|x| x.try_into().unwrap()).collect(),
        image_compression: image_compression.try_into().unwrap(),
        compression_rate_code: compression_rate_code.try_into().unwrap(),
        num_bands,
        image_sync_code,
        image_mode: image_mode.try_into().unwrap(),
        num_blocks_per_row,
        num_blocks_per_col,
        num_pixels_per_block_horz,
        num_pixels_per_block_vert,
        num_pixels_per_band,
        image_display_level,
        attachment_level,
        image_location,
        image_magnification: image_magnification.try_into().unwrap(),
        user_defined_image_data_length,
        user_defined_overflow,
        image_data_length,
    }))
}

fn parse_nitf(input: &[u8]) -> IResult<&[u8], NITF> {
    let (input, header) = parse_nitf_header(input)?;
    let (input, image_segments) = count(parse_image_segment, header.num_image_segments as usize)(input)?;
    
    Ok((input, NITF {
        header,
        image_segments,
    }))
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
        Err(e) => eprintln!("Failed to parse NITF: {:?}", e),
    }
}