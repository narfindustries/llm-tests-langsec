use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{many0, many1},
    number::complete::{be_u16, be_u32, be_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
struct FileHeader {
    file_header_length: u32,
    file_header_version: u16,
    file_type: String,
    file_security_classification: u8,
    file_control_number: String,
    file_date: String,
    file_time: String,
    file_title: String,
    file_security_classification_title: String,
    file_security_classification_text: String,
    file_security_classification_marking: u8,
    file_security_classification_downgrade: u8,
    file_security_classification_downgrade_date: Option<String>,
    file_security_classification_downgrade_time: Option<String>,
    file_security_classification_downgrade_authority: Option<String>,
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, file_header_length) = be_u32(input)?;
    let (input, file_header_version) = be_u16(input)?;
    let (input, file_type) = map(take(3u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, file_security_classification) = be_u8(input)?;
    let (input, file_control_number) = map(take(25u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, file_date) = map(take(6u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, file_time) = map(take(4u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, file_title) = map(take(80u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, file_security_classification_title) = map(take(43u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, file_security_classification_text) = map(take(80u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, file_security_classification_marking) = be_u8(input)?;
    let (input, file_security_classification_downgrade) = be_u8(input)?;
    let (input, file_security_classification_downgrade_date) = opt(map(take(6u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned()))(input)?;
    let (input, file_security_classification_downgrade_time) = opt(map(take(4u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned()))(input)?;
    let (input, file_security_classification_downgrade_authority) = opt(map(take(25u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned()))(input)?;
    Ok((
        input,
        FileHeader {
            file_header_length,
            file_header_version,
            file_type,
            file_security_classification,
            file_control_number,
            file_date,
            file_time,
            file_title,
            file_security_classification_title,
            file_security_classification_text,
            file_security_classification_marking,
            file_security_classification_downgrade,
            file_security_classification_downgrade_date,
            file_security_classification_downgrade_time,
            file_security_classification_downgrade_authority,
        },
    ))
}

#[derive(Debug, PartialEq)]
struct ImageHeader {
    image_id: String,
    image_date: String,
    image_time: String,
    image_title: String,
    image_security_classification: u8,
    image_security_classification_title: String,
    image_security_classification_text: String,
    image_security_classification_marking: u8,
    image_compression_type: u16,
    image_pixel_type: u8,
    image_pixel_size: u16,
    image_pixel_format: u16,
    image_number_of_bands: u8,
    image_band_definitions: Vec<String>,
}

fn parse_image_header(input: &[u8]) -> IResult<&[u8], ImageHeader> {
    let (input, image_id) = map(take(25u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, image_date) = map(take(6u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, image_time) = map(take(4u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, image_title) = map(take(80u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, image_security_classification) = be_u8(input)?;
    let (input, image_security_classification_title) = map(take(43u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, image_security_classification_text) = map(take(80u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned())(input)?;
    let (input, image_security_classification_marking) = be_u8(input)?;
    let (input, image_compression_type) = be_u16(input)?;
    let (input, image_pixel_type) = be_u8(input)?;
    let (input, image_pixel_size) = be_u16(input)?;
    let (input, image_pixel_format) = be_u16(input)?;
    let (input, image_number_of_bands) = be_u8(input)?;
    let (input, image_band_definitions) = many1(map(take(80u8), |x: &[u8]| String::from_utf8_lossy(x).into_owned()))(input)?;
    Ok((
        input,
        ImageHeader {
            image_id,
            image_date,
            image_time,
            image_title,
            image_security_classification,
            image_security_classification_title,
            image_security_classification_text,
            image_security_classification_marking,
            image_compression_type,
            image_pixel_type,
            image_pixel_size,
            image_pixel_format,
            image_number_of_bands,
            image_band_definitions,
        },
    ))
}

#[derive(Debug, PartialEq)]
struct DataDescriptionSection {
    data_description_section_length: u32,
    data_description_section_version: u16,
    data_type: u8,
    data_representation: u8,
    data_size: u32,
    data_offset: u32,
}

fn parse_data_description_section(input: &[u8]) -> IResult<&[u8], DataDescriptionSection> {
    let (input, data_description_section_length) = be_u32(input)?;
    let (input, data_description_section_version) = be_u16(input)?;
    let (input, data_type) = be_u8(input)?;
    let (input, data_representation) = be_u8(input)?;
    let (input, data_size) = be_u32(input)?;
    let (input, data_offset) = be_u32(input)?;
    Ok((
        input,
        DataDescriptionSection {
            data_description_section_length,
            data_description_section_version,
            data_type,
            data_representation,
            data_size,
            data_offset,
        },
    ))
}

#[derive(Debug, PartialEq)]
struct DataExtensionSection {
    data_extension_section_length: u32,
    data_extension_section_version: u16,
    data_extension_type: u8,
    data_extension_size: u32,
    data_extension_offset: u32,
}

fn parse_data_extension_section(input: &[u8]) -> IResult<&[u8], DataExtensionSection> {
    let (input, data_extension_section_length) = be_u32(input)?;
    let (input, data_extension_section_version) = be_u16(input)?;
    let (input, data_extension_type) = be_u8(input)?;
    let (input, data_extension_size) = be_u32(input)?;
    let (input, data_extension_offset) = be_u32(input)?;
    Ok((
        input,
        DataExtensionSection {
            data_extension_section_length,
            data_extension_section_version,
            data_extension_type,
            data_extension_size,
            data_extension_offset,
        },
    ))
}

#[derive(Debug, PartialEq)]
struct ImageDataSection {
    image_data_section_length: u32,
    image_data_section_version: u16,
    image_data_type: u8,
    image_data_size: u32,
    image_data_offset: u32,
}

fn parse_image_data_section(input: &[u8]) -> IResult<&[u8], ImageDataSection> {
    let (input, image_data_section_length) = be_u32(input)?;
    let (input, image_data_section_version) = be_u16(input)?;
    let (input, image_data_type) = be_u8(input)?;
    let (input, image_data_size) = be_u32(input)?;
    let (input, image_data_offset) = be_u32(input)?;
    Ok((
        input,
        ImageDataSection {
            image_data_section_length,
            image_data_section_version,
            image_data_type,
            image_data_size,
            image_data_offset,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let file = File::open(input_file).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (input, file_header) = parse_file_header(&input).unwrap();
    let (input, image_header) = parse_image_header(input).unwrap();
    let (input, data_description_section) = parse_data_description_section(input).unwrap();
    let (input, data_extension_section) = parse_data_extension_section(input).unwrap();
    let (input, image_data_section) = parse_image_data_section(input).unwrap();
    println!("File Header: {:?}", file_header);
    println!("Image Header: {:?}", image_header);
    println!("Data Description Section: {:?}", data_description_section);
    println!("Data Extension Section: {:?}", data_extension_section);
    println!("Image Data Section: {:?}", image_data_section);
}