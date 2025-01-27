use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct NITFHeader {
    file_header: FileHeader,
    image_segments: Vec<ImageSegment>,
    graphic_segments: Vec<GraphicSegment>,
    text_segments: Vec<TextSegment>,
    data_extension_segments: Vec<DataExtensionSegment>,
    reserved_extension_segments: Vec<ReservedExtensionSegment>,
}

#[derive(Debug)]
struct FileHeader {
    file_profile_name: String,
    file_version: String,
    complexity_level: String,
    standard_type: String,
    originating_station_id: String,
    file_date_and_time: String,
    file_title: String,
    file_security: FileSecurity,
    file_copy_number: String,
    file_number_of_copies: String,
    encryption: String,
    file_background_color: String,
    originators_name: String,
    originators_phone: String,
    file_length: u32,
    header_length: u16,
    number_of_image_segments: u16,
    number_of_graphic_segments: u16,
    number_of_text_segments: u16,
    number_of_data_extension_segments: u16,
    number_of_reserved_extension_segments: u16,
    user_defined_header_data_length: u16,
    extended_header_data_length: u16,
}

#[derive(Debug)]
struct FileSecurity {
    classification: String,
    classification_system: String,
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
struct ImageSegment {
    subheader: ImageSubheader,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ImageSubheader {
    image_id: String,
    image_date_and_time: String,
    target_id: String,
    image_title: String,
    image_security: FileSecurity,
    encryption: String,
    image_source: String,
    pixel_value_type: String,
    image_compression: String,
    image_coordinate_system: String,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct GraphicSegment {
    subheader: GraphicSubheader,
    data: Vec<u8>,
}

#[derive(Debug)]
struct GraphicSubheader {
    graphic_id: String,
    graphic_name: String,
    graphic_security: FileSecurity,
    encryption: String,
    graphic_type: String,
    reserved_field: String,
    graphic_data: Vec<u8>,
}

#[derive(Debug)]
struct TextSegment {
    subheader: TextSubheader,
    data: Vec<u8>,
}

#[derive(Debug)]
struct TextSubheader {
    text_id: String,
    text_date_and_time: String,
    text_title: String,
    text_security: FileSecurity,
    encryption: String,
    text_format: String,
    text_data: Vec<u8>,
}

#[derive(Debug)]
struct DataExtensionSegment {
    subheader: DataExtensionSubheader,
    data: Vec<u8>,
}

#[derive(Debug)]
struct DataExtensionSubheader {
    data_extension_id: String,
    data_extension_security: FileSecurity,
    encryption: String,
    data_extension_data: Vec<u8>,
}

#[derive(Debug)]
struct ReservedExtensionSegment {
    subheader: ReservedExtensionSubheader,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ReservedExtensionSubheader {
    reserved_extension_id: String,
    reserved_extension_security: FileSecurity,
    encryption: String,
    reserved_extension_data: Vec<u8>,
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, file_profile_name) = take(4u8)(input)?;
    let (input, file_version) = take(2u8)(input)?;
    let (input, complexity_level) = take(2u8)(input)?;
    let (input, standard_type) = take(4u8)(input)?;
    let (input, originating_station_id) = take(10u8)(input)?;
    let (input, file_date_and_time) = take(14u8)(input)?;
    let (input, file_title) = take(80u8)(input)?;
    let (input, file_security) = parse_file_security(input)?;
    let (input, file_copy_number) = take(1u8)(input)?;
    let (input, file_number_of_copies) = take(1u8)(input)?;
    let (input, encryption) = take(1u8)(input)?;
    let (input, file_background_color) = take(3u8)(input)?;
    let (input, originators_name) = take(24u8)(input)?;
    let (input, originators_phone) = take(18u8)(input)?;
    let (input, file_length) = be_u32(input)?;
    let (input, header_length) = be_u16(input)?;
    let (input, number_of_image_segments) = be_u16(input)?;
    let (input, number_of_graphic_segments) = be_u16(input)?;
    let (input, number_of_text_segments) = be_u16(input)?;
    let (input, number_of_data_extension_segments) = be_u16(input)?;
    let (input, number_of_reserved_extension_segments) = be_u16(input)?;
    let (input, user_defined_header_data_length) = be_u16(input)?;
    let (input, extended_header_data_length) = be_u16(input)?;

    Ok((
        input,
        FileHeader {
            file_profile_name: String::from_utf8_lossy(file_profile_name).to_string(),
            file_version: String::from_utf8_lossy(file_version).to_string(),
            complexity_level: String::from_utf8_lossy(complexity_level).to_string(),
            standard_type: String::from_utf8_lossy(standard_type).to_string(),
            originating_station_id: String::from_utf8_lossy(originating_station_id).to_string(),
            file_date_and_time: String::from_utf8_lossy(file_date_and_time).to_string(),
            file_title: String::from_utf8_lossy(file_title).to_string(),
            file_security,
            file_copy_number: String::from_utf8_lossy(file_copy_number).to_string(),
            file_number_of_copies: String::from_utf8_lossy(file_number_of_copies).to_string(),
            encryption: String::from_utf8_lossy(encryption).to_string(),
            file_background_color: String::from_utf8_lossy(file_background_color).to_string(),
            originators_name: String::from_utf8_lossy(originators_name).to_string(),
            originators_phone: String::from_utf8_lossy(originators_phone).to_string(),
            file_length,
            header_length,
            number_of_image_segments,
            number_of_graphic_segments,
            number_of_text_segments,
            number_of_data_extension_segments,
            number_of_reserved_extension_segments,
            user_defined_header_data_length,
            extended_header_data_length,
        },
    ))
}

fn parse_file_security(input: &[u8]) -> IResult<&[u8], FileSecurity> {
    let (input, classification) = take(1u8)(input)?;
    let (input, classification_system) = take(2u8)(input)?;
    let (input, codewords) = take(11u8)(input)?;
    let (input, control_and_handling) = take(2u8)(input)?;
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

    Ok((
        input,
        FileSecurity {
            classification: String::from_utf8_lossy(classification).to_string(),
            classification_system: String::from_utf8_lossy(classification_system).to_string(),
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
        },
    ))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, subheader) = parse_image_subheader(input)?;
    let (input, data) = take(subheader.image_data.len())(input)?;

    Ok((
        input,
        ImageSegment {
            subheader,
            data: data.to_vec(),
        },
    ))
}

fn parse_image_subheader(input: &[u8]) -> IResult<&[u8], ImageSubheader> {
    let (input, image_id) = take(10u8)(input)?;
    let (input, image_date_and_time) = take(14u8)(input)?;
    let (input, target_id) = take(17u8)(input)?;
    let (input, image_title) = take(80u8)(input)?;
    let (input, image_security) = parse_file_security(input)?;
    let (input, encryption) = take(1u8)(input)?;
    let (input, image_source) = take(42u8)(input)?;
    let (input, pixel_value_type) = take(3u8)(input)?;
    let (input, image_compression) = take(2u8)(input)?;
    let (input, image_coordinate_system) = take(1u8)(input)?;
    let (input, image_data) = take(0u8)(input)?; // Placeholder, actual length needs to be determined

    Ok((
        input,
        ImageSubheader {
            image_id: String::from_utf8_lossy(image_id).to_string(),
            image_date_and_time: String::from_utf8_lossy(image_date_and_time).to_string(),
            target_id: String::from_utf8_lossy(target_id).to_string(),
            image_title: String::from_utf8_lossy(image_title).to_string(),
            image_security,
            encryption: String::from_utf8_lossy(encryption).to_string(),
            image_source: String::from_utf8_lossy(image_source).to_string(),
            pixel_value_type: String::from_utf8_lossy(pixel_value_type).to_string(),
            image_compression: String::from_utf8_lossy(image_compression).to_string(),
            image_coordinate_system: String::from_utf8_lossy(image_coordinate_system).to_string(),
            image_data: image_data.to_vec(),
        },
    ))
}

fn parse_graphic_segment(input: &[u8]) -> IResult<&[u8], GraphicSegment> {
    let (input, subheader) = parse_graphic_subheader(input)?;
    let (input, data) = take(subheader.graphic_data.len())(input)?;

    Ok((
        input,
        GraphicSegment {
            subheader,
            data: data.to_vec(),
        },
    ))
}

fn parse_graphic_subheader(input: &[u8]) -> IResult<&[u8], GraphicSubheader> {
    let (input, graphic_id) = take(10u8)(input)?;
    let (input, graphic_name) = take(20u8)(input)?;
    let (input, graphic_security) = parse_file_security(input)?;
    let (input, encryption) = take(1u8)(input)?;
    let (input, graphic_type) = take(1u8)(input)?;
    let (input, reserved_field) = take(1u8)(input)?;
    let (input, graphic_data) = take(0u8)(input)?; // Placeholder, actual length needs to be determined

    Ok((
        input,
        GraphicSubheader {
            graphic_id: String::from_utf8_lossy(graphic_id).to_string(),
            graphic_name: String::from_utf8_lossy(graphic_name).to_string(),
            graphic_security,
            encryption: String::from_utf8_lossy(encryption).to_string(),
            graphic_type: String::from_utf8_lossy(graphic_type).to_string(),
            reserved_field: String::from_utf8_lossy(reserved_field).to_string(),
            graphic_data: graphic_data.to_vec(),
        },
    ))
}

fn parse_text_segment(input: &[u8]) -> IResult<&[u8], TextSegment> {
    let (input, subheader) = parse_text_subheader(input)?;
    let (input, data) = take(subheader.text_data.len())(input)?;

    Ok((
        input,
        TextSegment {
            subheader,
            data: data.to_vec(),
        },
    ))
}

fn parse_text_subheader(input: &[u8]) -> IResult<&[u8], TextSubheader> {
    let (input, text_id) = take(10u8)(input)?;
    let (input, text_date_and_time) = take(14u8)(input)?;
    let (input, text_title) = take(80u8)(input)?;
    let (input, text_security) = parse_file_security(input)?;
    let (input, encryption) = take(1u8)(input)?;
    let (input, text_format) = take(3u8)(input)?;
    let (input, text_data) = take(0u8)(input)?; // Placeholder, actual length needs to be determined

    Ok((
        input,
        TextSubheader {
            text_id: String::from_utf8_lossy(text_id).to_string(),
            text_date_and_time: String::from_utf8_lossy(text_date_and_time).to_string(),
            text_title: String::from_utf8_lossy(text_title).to_string(),
            text_security,
            encryption: String::from_utf8_lossy(encryption).to_string(),
            text_format: String::from_utf8_lossy(text_format).to_string(),
            text_data: text_data.to_vec(),
        },
    ))
}

fn parse_data_extension_segment(input: &[u8]) -> IResult<&[u8], DataExtensionSegment> {
    let (input, subheader) = parse_data_extension_subheader(input)?;
    let (input, data) = take(subheader.data_extension_data.len())(input)?;

    Ok((
        input,
        DataExtensionSegment {
            subheader,
            data: data.to_vec(),
        },
    ))
}

fn parse_data_extension_subheader(input: &[u8]) -> IResult<&[u8], DataExtensionSubheader> {
    let (input, data_extension_id) = take(10u8)(input)?;
    let (input, data_extension_security) = parse_file_security(input)?;
    let (input, encryption) = take(1u8)(input)?;
    let (input, data_extension_data) = take(0u8)(input)?; // Placeholder, actual length needs to be determined

    Ok((
        input,
        DataExtensionSubheader {
            data_extension_id: String::from_utf8_lossy(data_extension_id).to_string(),
            data_extension_security,
            encryption: String::from_utf8_lossy(encryption).to_string(),
            data_extension_data: data_extension_data.to_vec(),
        },
    ))
}

fn parse_reserved_extension_segment(input: &[u8]) -> IResult<&[u8], ReservedExtensionSegment> {
    let (input, subheader) = parse_reserved_extension_subheader(input)?;
    let (input, data) = take(subheader.reserved_extension_data.len())(input)?;

    Ok((
        input,
        ReservedExtensionSegment {
            subheader,
            data: data.to_vec(),
        },
    ))
}

fn parse_reserved_extension_subheader(input: &[u8]) -> IResult<&[u8], ReservedExtensionSubheader> {
    let (input, reserved