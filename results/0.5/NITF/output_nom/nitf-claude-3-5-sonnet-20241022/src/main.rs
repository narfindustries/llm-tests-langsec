use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NitfHeader {
    file_header_length: u32,
    file_length: u32,
    header_type: String,
    version: String,
    complexity_level: u8,
    system_type: String,
    originating_station: String,
    date_time: String,
    title: String,
    security_classification: char,
    copy_number: String,
    num_image_segments: u16,
    num_graphic_segments: u16,
    num_text_segments: u16,
    num_data_extension_segments: u16,
    num_reserved_extension_segments: u16,
}

#[derive(Debug)]
struct ImageSegment {
    subheader_length: u32,
    data_length: u32,
    image_identifier: String,
    date_time: String,
    target_identifier: String,
    image_identifier2: String,
    security_classification: char,
    encryption: String,
    image_source: String,
    num_significant_rows: u32,
    num_significant_cols: u32,
    pixel_value_type: String,
    image_representation: String,
    image_category: String,
    actual_bits_per_pixel: u8,
    pixel_justification: String,
    image_coordinate_system: String,
    image_compression: String,
    compression_rate_code: String,
    num_bands: u8,
    image_sync_code: u8,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, (
        _file_profile,
        _file_version,
        _complexity_level,
        _standard_type,
        file_header_length,
        file_length,
        _header_type,
        _version,
        _system_type,
        _originating_station,
        _date_time,
        _title,
        _security_classification,
        _copy_number,
        num_image_segments,
        num_graphic_segments,
        num_text_segments,
        num_data_extension_segments,
        num_reserved_extension_segments,
    )) = tuple((
        take(4usize),
        take(5usize),
        be_u8,
        take(2usize),
        be_u32,
        be_u32,
        take(2usize),
        take(5usize),
        take(10usize),
        take(14usize),
        take(14usize),
        take(80usize),
        take(1usize),
        take(5usize),
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u16,
    ))(input)?;

    Ok((input, NitfHeader {
        file_header_length,
        file_length,
        header_type: String::from_utf8_lossy(&[0; 2]).into_owned(),
        version: String::from_utf8_lossy(&[0; 5]).into_owned(),
        complexity_level: 0,
        system_type: String::from_utf8_lossy(&[0; 10]).into_owned(),
        originating_station: String::from_utf8_lossy(&[0; 14]).into_owned(),
        date_time: String::from_utf8_lossy(&[0; 14]).into_owned(),
        title: String::from_utf8_lossy(&[0; 80]).into_owned(),
        security_classification: ' ',
        copy_number: String::from_utf8_lossy(&[0; 5]).into_owned(),
        num_image_segments,
        num_graphic_segments,
        num_text_segments,
        num_data_extension_segments,
        num_reserved_extension_segments,
    }))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, (
        subheader_length,
        data_length,
        _image_identifier,
        _date_time,
        _target_identifier,
        _image_identifier2,
        _security_classification,
        _encryption,
        _image_source,
        num_significant_rows,
        num_significant_cols,
        _pixel_value_type,
        _image_representation,
        _image_category,
        actual_bits_per_pixel,
        _pixel_justification,
        _image_coordinate_system,
        _image_compression,
        _compression_rate_code,
        num_bands,
        image_sync_code,
    )) = tuple((
        be_u32,
        be_u32,
        take(10usize),
        take(14usize),
        take(17usize),
        take(80usize),
        take(1usize),
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
        take(2usize),
        take(4usize),
        be_u8,
        be_u8,
    ))(input)?;

    Ok((input, ImageSegment {
        subheader_length,
        data_length,
        image_identifier: String::from_utf8_lossy(&[0; 10]).into_owned(),
        date_time: String::from_utf8_lossy(&[0; 14]).into_owned(),
        target_identifier: String::from_utf8_lossy(&[0; 17]).into_owned(),
        image_identifier2: String::from_utf8_lossy(&[0; 80]).into_owned(),
        security_classification: ' ',
        encryption: String::from_utf8_lossy(&[0; 1]).into_owned(),
        image_source: String::from_utf8_lossy(&[0; 42]).into_owned(),
        num_significant_rows,
        num_significant_cols,
        pixel_value_type: String::from_utf8_lossy(&[0; 3]).into_owned(),
        image_representation: String::from_utf8_lossy(&[0; 8]).into_owned(),
        image_category: String::from_utf8_lossy(&[0; 8]).into_owned(),
        actual_bits_per_pixel,
        pixel_justification: String::from_utf8_lossy(&[0; 1]).into_owned(),
        image_coordinate_system: String::from_utf8_lossy(&[0; 1]).into_owned(),
        image_compression: String::from_utf8_lossy(&[0; 2]).into_owned(),
        compression_rate_code: String::from_utf8_lossy(&[0; 4]).into_owned(),
        num_bands,
        image_sync_code,
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
        Ok((remaining, header)) => {
            println!("NITF Header: {:?}", header);
            
            if header.num_image_segments > 0 {
                match parse_image_segment(remaining) {
                    Ok((_, image_segment)) => {
                        println!("Image Segment: {:?}", image_segment);
                    }
                    Err(e) => eprintln!("Error parsing image segment: {:?}", e),
                }
            }
        }
        Err(e) => eprintln!("Error parsing NITF header: {:?}", e),
    }

    Ok(())
}