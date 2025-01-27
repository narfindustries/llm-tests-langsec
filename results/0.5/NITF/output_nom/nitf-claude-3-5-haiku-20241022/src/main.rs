use nom::{
    bytes::complete::{tag, take, take_until},
    combinator::{map, opt},
    multi::{count, many0, many_m_n},
    number::complete::{be_u8, be_u16, be_u32, be_u64},
    sequence::{tuple, preceded},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct NitfFile {
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
    complexity_level: u8,
    standard_type: String,
    originating_station: String,
    file_datetime: String,
    file_title: String,
    security_classification: SecurityClassification,
    file_copy_number: u16,
    file_number_of_copies: u16,
    encryption_status: u8,
}

#[derive(Debug)]
struct SecurityClassification {
    security_class: String,
    security_system: String,
    codewords: String,
    control_and_handling: String,
    releasing_instructions: String,
}

#[derive(Debug)]
struct ImageSegment {
    image_subheader: ImageSubheader,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct ImageSubheader {
    image_identifier: String,
    image_datetime: String,
    image_title: String,
    security_classification: SecurityClassification,
}

#[derive(Debug)]
struct GraphicSegment {
    graphic_subheader: GraphicSubheader,
    graphic_data: Vec<u8>,
}

#[derive(Debug)]
struct GraphicSubheader {
    graphic_identifier: String,
    graphic_name: String,
}

#[derive(Debug)]
struct TextSegment {
    text_subheader: TextSubheader,
    text_data: Vec<u8>,
}

#[derive(Debug)]
struct TextSubheader {
    text_identifier: String,
    text_title: String,
}

#[derive(Debug)]
struct DataExtensionSegment {
    data_extension_subheader: DataExtensionSubheader,
    data_extension_data: Vec<u8>,
}

#[derive(Debug)]
struct DataExtensionSubheader {
    data_extension_type: String,
}

#[derive(Debug)]
struct ReservedExtensionSegment {
    reserved_extension_subheader: ReservedExtensionSubheader,
    reserved_extension_data: Vec<u8>,
}

#[derive(Debug)]
struct ReservedExtensionSubheader {
    reserved_extension_type: String,
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, file_profile_name) = take(4usize)(input)?;
    let (input, file_version) = take(4usize)(input)?;
    let (input, complexity_level) = be_u8(input)?;
    let (input, standard_type) = take(4usize)(input)?;
    let (input, originating_station) = take(10usize)(input)?;
    let (input, file_datetime) = take(14usize)(input)?;
    let (input, file_title) = take(80usize)(input)?;
    let (input, security_classification) = parse_security_classification(input)?;
    let (input, file_copy_number) = be_u16(input)?;
    let (input, file_number_of_copies) = be_u16(input)?;
    let (input, encryption_status) = be_u8(input)?;

    Ok((input, FileHeader {
        file_profile_name: String::from_utf8_lossy(file_profile_name).to_string(),
        file_version: String::from_utf8_lossy(file_version).to_string(),
        complexity_level,
        standard_type: String::from_utf8_lossy(standard_type).to_string(),
        originating_station: String::from_utf8_lossy(originating_station).to_string(),
        file_datetime: String::from_utf8_lossy(file_datetime).to_string(),
        file_title: String::from_utf8_lossy(file_title).to_string(),
        security_classification,
        file_copy_number,
        file_number_of_copies,
        encryption_status,
    }))
}

fn parse_security_classification(input: &[u8]) -> IResult<&[u8], SecurityClassification> {
    let (input, security_class) = take(1usize)(input)?;
    let (input, security_system) = take(2usize)(input)?;
    let (input, codewords) = take(11usize)(input)?;
    let (input, control_and_handling) = take(2usize)(input)?;
    let (input, releasing_instructions) = take(20usize)(input)?;

    Ok((input, SecurityClassification {
        security_class: String::from_utf8_lossy(security_class).to_string(),
        security_system: String::from_utf8_lossy(security_system).to_string(),
        codewords: String::from_utf8_lossy(codewords).to_string(),
        control_and_handling: String::from_utf8_lossy(control_and_handling).to_string(),
        releasing_instructions: String::from_utf8_lossy(releasing_instructions).to_string(),
    }))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, image_subheader) = parse_image_subheader(input)?;
    let image_data_length = image_subheader.image_title.len(); // Simplified, actual length calculation needed
    let (input, image_data) = take(image_data_length)(input)?;

    Ok((input, ImageSegment {
        image_subheader,
        image_data: image_data.to_vec(),
    }))
}

fn parse_image_subheader(input: &[u8]) -> IResult<&[u8], ImageSubheader> {
    let (input, image_identifier) = take(10usize)(input)?;
    let (input, image_datetime) = take(14usize)(input)?;
    let (input, image_title) = take(80usize)(input)?;
    let (input, security_classification) = parse_security_classification(input)?;

    Ok((input, ImageSubheader {
        image_identifier: String::from_utf8_lossy(image_identifier).to_string(),
        image_datetime: String::from_utf8_lossy(image_datetime).to_string(),
        image_title: String::from_utf8_lossy(image_title).to_string(),
        security_classification,
    }))
}

fn parse_nitf_file(input: &[u8]) -> IResult<&[u8], NitfFile> {
    let (input, file_header) = parse_file_header(input)?;
    let (input, image_segments) = many0(parse_image_segment)(input)?;
    let (input, graphic_segments) = many0(parse_graphic_segment)(input)?;
    let (input, text_segments) = many0(parse_text_segment)(input)?;
    let (input, data_extension_segments) = many0(parse_data_extension_segment)(input)?;
    let (input, reserved_extension_segments) = many0(parse_reserved_extension_segment)(input)?;

    Ok((input, NitfFile {
        file_header,
        image_segments,
        graphic_segments,
        text_segments,
        data_extension_segments,
        reserved_extension_segments,
    }))
}

fn parse_graphic_segment(input: &[u8]) -> IResult<&[u8], GraphicSegment> {
    let (input, graphic_subheader) = parse_graphic_subheader(input)?;
    let graphic_data_length = 0; // Simplified, actual length calculation needed
    let (input, graphic_data) = take(graphic_data_length)(input)?;

    Ok((input, GraphicSegment {
        graphic_subheader,
        graphic_data: graphic_data.to_vec(),
    }))
}

fn parse_graphic_subheader(input: &[u8]) -> IResult<&[u8], GraphicSubheader> {
    let (input, graphic_identifier) = take(10usize)(input)?;
    let (input, graphic_name) = take(20usize)(input)?;

    Ok((input, GraphicSubheader {
        graphic_identifier: String::from_utf8_lossy(graphic_identifier).to_string(),
        graphic_name: String::from_utf8_lossy(graphic_name).to_string(),
    }))
}

fn parse_text_segment(input: &[u8]) -> IResult<&[u8], TextSegment> {
    let (input, text_subheader) = parse_text_subheader(input)?;
    let text_data_length = 0; // Simplified, actual length calculation needed
    let (input, text_data) = take(text_data_length)(input)?;

    Ok((input, TextSegment {
        text_subheader,
        text_data: text_data.to_vec(),
    }))
}

fn parse_text_subheader(input: &[u8]) -> IResult<&[u8], TextSubheader> {
    let (input, text_identifier) = take(10usize)(input)?;
    let (input, text_title) = take(80usize)(input)?;

    Ok((input, TextSubheader {
        text_identifier: String::from_utf8_lossy(text_identifier).to_string(),
        text_title: String::from_utf8_lossy(text_title).to_string(),
    }))
}

fn parse_data_extension_segment(input: &[u8]) -> IResult<&[u8], DataExtensionSegment> {
    let (input, data_extension_subheader) = parse_data_extension_subheader(input)?;
    let data_extension_data_length = 0; // Simplified, actual length calculation needed
    let (input, data_extension_data) = take(data_extension_data_length)(input)?;

    Ok((input, DataExtensionSegment {
        data_extension_subheader,
        data_extension_data: data_extension_data.to_vec(),
    }))
}

fn parse_data_extension_subheader(input: &[u8]) -> IResult<&[u8], DataExtensionSubheader> {
    let (input, data_extension_type) = take(2usize)(input)?;

    Ok((input, DataExtensionSubheader {
        data_extension_type: String::from_utf8_lossy(data_extension_type).to_string(),
    }))
}

fn parse_reserved_extension_segment(input: &[u8]) -> IResult<&[u8], ReservedExtensionSegment> {
    let (input, reserved_extension_subheader) = parse_reserved_extension_subheader(input)?;
    let reserved_extension_data_length = 0; // Simplified, actual length calculation needed
    let (input, reserved_extension_data) = take(reserved_extension_data_length)(input)?;

    Ok((input, ReservedExtensionSegment {
        reserved_extension_subheader,
        reserved_extension_data: reserved_extension_data.to_vec(),
    }))
}

fn parse_reserved_extension_subheader(input: &[u8]) -> IResult<&[u8], ReservedExtensionSubheader> {
    let (input, reserved_extension_type) = take(2usize)(input)?;

    Ok((input, ReservedExtensionSubheader {
        reserved_extension_type: String::from_utf8_lossy(reserved_extension_type).to_string(),
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

    match parse_nitf_file(&buffer) {
        Ok((_, nitf_file)) => {
            println!("Parsed NITF file: {:?}", nitf_file);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            Err(e.into())
        }
    }
}