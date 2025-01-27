use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct NITFHeader {
    file_profile_name: String,
    file_version: String,
    complexity_level: u8,
    standard_type: String,
    originating_station_id: String,
    file_date_and_time: String,
    file_title: String,
    file_security: FileSecurity,
    file_copy_number: u16,
    file_number_of_parts: u16,
    file_background_color: [u8; 3],
    file_originators_name: String,
    file_originators_phone: String,
    file_length: u64,
    header_length: u32,
    number_of_image_segments: u16,
    image_segment_headers: Vec<ImageSegmentHeader>,
    number_of_graphic_segments: u16,
    graphic_segment_headers: Vec<GraphicSegmentHeader>,
    number_of_text_segments: u16,
    text_segment_headers: Vec<TextSegmentHeader>,
    number_of_data_extension_segments: u16,
    data_extension_segment_headers: Vec<DataExtensionSegmentHeader>,
    number_of_reserved_extension_segments: u16,
    reserved_extension_segment_headers: Vec<ReservedExtensionSegmentHeader>,
}

#[derive(Debug)]
struct FileSecurity {
    classification: String,
    codewords: String,
    control_and_handling: String,
    releaseability: String,
    declass_type: String,
    declass_date: String,
    declass_exemption: String,
    downgrade: String,
    downgrade_date: String,
    classification_text: String,
    classification_authority_type: String,
    classification_authority: String,
    classification_reason: String,
    security_source_date: String,
    security_control_number: String,
}

#[derive(Debug)]
struct ImageSegmentHeader {
    image_segment_id: String,
    image_date_and_time: String,
    image_title: String,
    image_security: FileSecurity,
    image_compression: String,
    image_compression_rate_code: String,
    image_location: u64,
    image_length: u64,
}

#[derive(Debug)]
struct GraphicSegmentHeader {
    graphic_segment_id: String,
    graphic_date_and_time: String,
    graphic_title: String,
    graphic_security: FileSecurity,
    graphic_location: u64,
    graphic_length: u64,
}

#[derive(Debug)]
struct TextSegmentHeader {
    text_segment_id: String,
    text_date_and_time: String,
    text_title: String,
    text_security: FileSecurity,
    text_location: u64,
    text_length: u64,
}

#[derive(Debug)]
struct DataExtensionSegmentHeader {
    data_extension_segment_id: String,
    data_extension_date_and_time: String,
    data_extension_title: String,
    data_extension_security: FileSecurity,
    data_extension_location: u64,
    data_extension_length: u64,
}

#[derive(Debug)]
struct ReservedExtensionSegmentHeader {
    reserved_extension_segment_id: String,
    reserved_extension_date_and_time: String,
    reserved_extension_title: String,
    reserved_extension_security: FileSecurity,
    reserved_extension_location: u64,
    reserved_extension_length: u64,
}

fn parse_file_security(input: &[u8]) -> IResult<&[u8], FileSecurity> {
    let (input, classification) = take(1u8)(input)?;
    let (input, codewords) = take(40u8)(input)?;
    let (input, control_and_handling) = take(40u8)(input)?;
    let (input, releaseability) = take(20u8)(input)?;
    let (input, declass_type) = take(2u8)(input)?;
    let (input, declass_date) = take(8u8)(input)?;
    let (input, declass_exemption) = take(4u8)(input)?;
    let (input, downgrade) = take(1u8)(input)?;
    let (input, downgrade_date) = take(8u8)(input)?;
    let (input, classification_text) = take(43u8)(input)?;
    let (input, classification_authority_type) = take(1u8)(input)?;
    let (input, classification_authority) = take(40u8)(input)?;
    let (input, classification_reason) = take(1u8)(input)?;
    let (input, security_source_date) = take(8u8)(input)?;
    let (input, security_control_number) = take(15u8)(input)?;

    Ok((input, FileSecurity {
        classification: String::from_utf8_lossy(classification).to_string(),
        codewords: String::from_utf8_lossy(codewords).to_string(),
        control_and_handling: String::from_utf8_lossy(control_and_handling).to_string(),
        releaseability: String::from_utf8_lossy(releaseability).to_string(),
        declass_type: String::from_utf8_lossy(declass_type).to_string(),
        declass_date: String::from_utf8_lossy(declass_date).to_string(),
        declass_exemption: String::from_utf8_lossy(declass_exemption).to_string(),
        downgrade: String::from_utf8_lossy(downgrade).to_string(),
        downgrade_date: String::from_utf8_lossy(downgrade_date).to_string(),
        classification_text: String::from_utf8_lossy(classification_text).to_string(),
        classification_authority_type: String::from_utf8_lossy(classification_authority_type).to_string(),
        classification_authority: String::from_utf8_lossy(classification_authority).to_string(),
        classification_reason: String::from_utf8_lossy(classification_reason).to_string(),
        security_source_date: String::from_utf8_lossy(security_source_date).to_string(),
        security_control_number: String::from_utf8_lossy(security_control_number).to_string(),
    }))
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, file_profile_name) = take(4u8)(input)?;
    let (input, file_version) = take(5u8)(input)?;
    let (input, complexity_level) = be_u8(input)?;
    let (input, standard_type) = take(12u8)(input)?;
    let (input, originating_station_id) = take(10u8)(input)?;
    let (input, file_date_and_time) = take(14u8)(input)?;
    let (input, file_title) = take(80u8)(input)?;
    let (input, file_security) = parse_file_security(input)?;
    let (input, file_copy_number) = be_u16(input)?;
    let (input, file_number_of_parts) = be_u16(input)?;
    let (input, file_background_color) = take(3u8)(input)?;
    let (input, file_originators_name) = take(24u8)(input)?;
    let (input, file_originators_phone) = take(18u8)(input)?;
    let (input, file_length) = be_u64(input)?;
    let (input, header_length) = be_u32(input)?;
    let (input, number_of_image_segments) = be_u16(input)?;
    let (input, image_segment_headers) = nom::multi::count(parse_image_segment_header, number_of_image_segments as usize)(input)?;
    let (input, number_of_graphic_segments) = be_u16(input)?;
    let (input, graphic_segment_headers) = nom::multi::count(parse_graphic_segment_header, number_of_graphic_segments as usize)(input)?;
    let (input, number_of_text_segments) = be_u16(input)?;
    let (input, text_segment_headers) = nom::multi::count(parse_text_segment_header, number_of_text_segments as usize)(input)?;
    let (input, number_of_data_extension_segments) = be_u16(input)?;
    let (input, data_extension_segment_headers) = nom::multi::count(parse_data_extension_segment_header, number_of_data_extension_segments as usize)(input)?;
    let (input, number_of_reserved_extension_segments) = be_u16(input)?;
    let (input, reserved_extension_segment_headers) = nom::multi::count(parse_reserved_extension_segment_header, number_of_reserved_extension_segments as usize)(input)?;

    Ok((input, NITFHeader {
        file_profile_name: String::from_utf8_lossy(file_profile_name).to_string(),
        file_version: String::from_utf8_lossy(file_version).to_string(),
        complexity_level,
        standard_type: String::from_utf8_lossy(standard_type).to_string(),
        originating_station_id: String::from_utf8_lossy(originating_station_id).to_string(),
        file_date_and_time: String::from_utf8_lossy(file_date_and_time).to_string(),
        file_title: String::from_utf8_lossy(file_title).to_string(),
        file_security,
        file_copy_number,
        file_number_of_parts,
        file_background_color: [file_background_color[0], file_background_color[1], file_background_color[2]],
        file_originators_name: String::from_utf8_lossy(file_originators_name).to_string(),
        file_originators_phone: String::from_utf8_lossy(file_originators_phone).to_string(),
        file_length,
        header_length,
        number_of_image_segments,
        image_segment_headers,
        number_of_graphic_segments,
        graphic_segment_headers,
        number_of_text_segments,
        text_segment_headers,
        number_of_data_extension_segments,
        data_extension_segment_headers,
        number_of_reserved_extension_segments,
        reserved_extension_segment_headers,
    }))
}

fn parse_image_segment_header(input: &[u8]) -> IResult<&[u8], ImageSegmentHeader> {
    let (input, image_segment_id) = take(10u8)(input)?;
    let (input, image_date_and_time) = take(14u8)(input)?;
    let (input, image_title) = take(80u8)(input)?;
    let (input, image_security) = parse_file_security(input)?;
    let (input, image_compression) = take(2u8)(input)?;
    let (input, image_compression_rate_code) = take(4u8)(input)?;
    let (input, image_location) = be_u64(input)?;
    let (input, image_length) = be_u64(input)?;

    Ok((input, ImageSegmentHeader {
        image_segment_id: String::from_utf8_lossy(image_segment_id).to_string(),
        image_date_and_time: String::from_utf8_lossy(image_date_and_time).to_string(),
        image_title: String::from_utf8_lossy(image_title).to_string(),
        image_security,
        image_compression: String::from_utf8_lossy(image_compression).to_string(),
        image_compression_rate_code: String::from_utf8_lossy(image_compression_rate_code).to_string(),
        image_location,
        image_length,
    }))
}

fn parse_graphic_segment_header(input: &[u8]) -> IResult<&[u8], GraphicSegmentHeader> {
    let (input, graphic_segment_id) = take(10u8)(input)?;
    let (input, graphic_date_and_time) = take(14u8)(input)?;
    let (input, graphic_title) = take(80u8)(input)?;
    let (input, graphic_security) = parse_file_security(input)?;
    let (input, graphic_location) = be_u64(input)?;
    let (input, graphic_length) = be_u64(input)?;

    Ok((input, GraphicSegmentHeader {
        graphic_segment_id: String::from_utf8_lossy(graphic_segment_id).to_string(),
        graphic_date_and_time: String::from_utf8_lossy(graphic_date_and_time).to_string(),
        graphic_title: String::from_utf8_lossy(graphic_title).to_string(),
        graphic_security,
        graphic_location,
        graphic_length,
    }))
}

fn parse_text_segment_header(input: &[u8]) -> IResult<&[u8], TextSegmentHeader> {
    let (input, text_segment_id) = take(10u8)(input)?;
    let (input, text_date_and_time) = take(14u8)(input)?;
    let (input, text_title) = take(80u8)(input)?;
    let (input, text_security) = parse_file_security(input)?;
    let (input, text_location) = be_u64(input)?;
    let (input, text_length) = be_u64(input)?;

    Ok((input, TextSegmentHeader {
        text_segment_id: String::from_utf8_lossy(text_segment_id).to_string(),
        text_date_and_time: String::from_utf8_lossy(text_date_and_time).to_string(),
        text_title: String::from_utf8_lossy(text_title).to_string(),
        text_security,
        text_location,
        text_length,
    }))
}

fn parse_data_extension_segment_header(input: &[u8]) -> IResult<&[u8], DataExtensionSegmentHeader> {
    let (input, data_extension_segment_id) = take(10u8)(input)?;
    let (input, data_extension_date_and_time) = take(14u8)(input)?;
    let (input, data_extension_title) = take(80u8)(input)?;
    let (input, data_extension_security) = parse_file_security(input)?;
    let (input, data_extension_location) = be_u64(input)?;
    let (input, data_extension_length) = be_u64(input)?;

    Ok((input, DataExtensionSegmentHeader {
        data_extension_segment_id: String::from_utf8_lossy(data_extension_segment_id).to_string(),
        data_extension_date_and_time: String::from_utf8_lossy(data_extension_date_and_time).to_string(),
        data_extension_title: String::from_utf8_lossy(data_extension_title).to_string(),
        data_extension_security,
        data_extension_location,
        data_extension_length,
    }))
}

fn parse_reserved_extension_segment_header(input: &[u8]) -> IResult<&[u8], ReservedExtensionSegmentHeader> {
    let (input, reserved_extension_segment_id) = take(10u8)(input)?;
    let (input, reserved_extension_date_and_time) = take(14u8)(input)?;
    let (input, reserved_extension_title) = take(80u8)(input)?;
    let (input, reserved_extension_security) = parse_file_security(input)?;
    let (input, reserved_extension_location) = be_u64(input)?;
    let (input, reserved_extension_length) = be_u64(input)?;

    Ok((input, ReservedExtensionSegmentHeader {
        reserved_extension_segment_id: String::from_utf8_lossy(reserved_extension_segment_id).to_string(),
        reserved_extension_date_and_time: String::from_utf8_lossy(reserved_extension_date_and_time).to_string(),
        reserved_extension_title: String::from_utf8_lossy(reserved_extension_title).to_string(),
        reserved_extension_security,
        reserved_extension_location,
        reserved_extension_length,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_nitf_header(&buffer) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(e) => eprintln!("Failed to parse NITF header: {:?}", e),
    }
}