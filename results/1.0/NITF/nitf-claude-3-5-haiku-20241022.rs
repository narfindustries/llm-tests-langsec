use nom::{
    bytes::complete::{tag, take, take_until},
    multi::{count, many0, many_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{tuple, preceded},
    IResult, sequence::pair,
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
    originating_station: String,
    file_date_time: String,
    file_title: Option<String>,
    security_group: SecurityGroup,
    extended_headers: Vec<ExtendedHeader>,
    image_segments: Vec<ImageSegment>,
}

#[derive(Debug)]
struct SecurityGroup {
    security_classification: String,
    security_system: String,
    codewords: Option<String>,
    control_and_handling: Option<String>,
    releasing_instructions: Option<String>,
    declassification_date: Option<String>,
    declassification_exemption: Option<String>,
    downgrade: Option<String>,
    downgrade_date: Option<String>,
}

#[derive(Debug)]
struct ExtendedHeader {
    header_data_type: String,
    header_data_length: u32,
    header_data: Vec<u8>,
}

#[derive(Debug)]
struct ImageSegment {
    header: ImageSegmentHeader,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct ImageSegmentHeader {
    length: u32,
    image_identifier: String,
    image_datetime: String,
    target_identifier: Option<String>,
    image_title: Option<String>,
    security_group: SecurityGroup,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, file_profile_name) = take(2usize)(input)?;
    let (input, file_version) = take(2usize)(input)?;
    let (input, complexity_level) = be_u8(input)?;
    let (input, standard_type) = take(4usize)(input)?;
    let (input, originating_station) = take(10usize)(input)?;
    let (input, file_date_time) = take(14usize)(input)?;
    let (input, file_title) = parse_optional_field(11)(input)?;
    let (input, security_group) = parse_security_group(input)?;
    let (input, extended_headers) = parse_extended_headers(input)?;
    let (input, image_segments) = parse_image_segments(input)?;

    Ok((input, NITFHeader {
        file_profile_name: String::from_utf8_lossy(file_profile_name).to_string(),
        file_version: String::from_utf8_lossy(file_version).to_string(),
        complexity_level,
        standard_type: String::from_utf8_lossy(standard_type).to_string(),
        originating_station: String::from_utf8_lossy(originating_station).to_string(),
        file_date_time: String::from_utf8_lossy(file_date_time).to_string(),
        file_title: file_title.map(|f| String::from_utf8_lossy(&f).to_string()),
        security_group,
        extended_headers,
        image_segments,
    }))
}

fn parse_security_group(input: &[u8]) -> IResult<&[u8], SecurityGroup> {
    let (input, security_classification) = take(1usize)(input)?;
    let (input, security_system) = take(2usize)(input)?;
    let (input, codewords) = parse_optional_field(11)(input)?;
    let (input, control_and_handling) = parse_optional_field(11)(input)?;
    let (input, releasing_instructions) = parse_optional_field(11)(input)?;
    let (input, declassification_date) = parse_optional_field(8)(input)?;
    let (input, declassification_exemption) = parse_optional_field(4)(input)?;
    let (input, downgrade) = parse_optional_field(1)(input)?;
    let (input, downgrade_date) = parse_optional_field(8)(input)?;

    Ok((input, SecurityGroup {
        security_classification: String::from_utf8_lossy(security_classification).to_string(),
        security_system: String::from_utf8_lossy(security_system).to_string(),
        codewords: codewords.map(|c| String::from_utf8_lossy(&c).to_string()),
        control_and_handling: control_and_handling.map(|c| String::from_utf8_lossy(&c).to_string()),
        releasing_instructions: releasing_instructions.map(|r| String::from_utf8_lossy(&r).to_string()),
        declassification_date: declassification_date.map(|d| String::from_utf8_lossy(&d).to_string()),
        declassification_exemption: declassification_exemption.map(|d| String::from_utf8_lossy(&d).to_string()),
        downgrade: downgrade.map(|d| String::from_utf8_lossy(&d).to_string()),
        downgrade_date: downgrade_date.map(|d| String::from_utf8_lossy(&d).to_string()),
    }))
}

fn parse_optional_field(length: usize) -> impl Fn(&[u8]) -> IResult<&[u8], Option<Vec<u8>>> {
    move |input| {
        let (input, field) = take(length)(input)?;
        let optional_field = if field.iter().all(|&b| b == b' ') { None } else { Some(field.to_vec()) };
        Ok((input, optional_field))
    }
}

fn parse_extended_headers(input: &[u8]) -> IResult<&[u8], Vec<ExtendedHeader>> {
    let (input, num_extended_headers) = be_u8(input)?;
    let (input, extended_headers) = count(parse_extended_header, num_extended_headers as usize)(input)?;
    Ok((input, extended_headers))
}

fn parse_extended_header(input: &[u8]) -> IResult<&[u8], ExtendedHeader> {
    let (input, header_data_type) = take(2usize)(input)?;
    let (input, header_data_length) = be_u32(input)?;
    let (input, header_data) = take(header_data_length as usize)(input)?;

    Ok((input, ExtendedHeader {
        header_data_type: String::from_utf8_lossy(header_data_type).to_string(),
        header_data_length,
        header_data: header_data.to_vec(),
    }))
}

fn parse_image_segments(input: &[u8]) -> IResult<&[u8], Vec<ImageSegment>> {
    let (input, num_image_segments) = be_u8(input)?;
    let (input, image_segments) = count(parse_image_segment, num_image_segments as usize)(input)?;
    Ok((input, image_segments))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, header) = parse_image_segment_header(input)?;
    let (input, image_data) = take(header.length)(input)?;

    Ok((input, ImageSegment {
        header,
        image_data: image_data.to_vec(),
    }))
}

fn parse_image_segment_header(input: &[u8]) -> IResult<&[u8], ImageSegmentHeader> {
    let (input, length) = be_u32(input)?;
    let (input, image_identifier) = take(10usize)(input)?;
    let (input, image_datetime) = take(14usize)(input)?;
    let (input, target_identifier) = parse_optional_field(17)(input)?;
    let (input, image_title) = parse_optional_field(80)(input)?;
    let (input, security_group) = parse_security_group(input)?;

    Ok((input, ImageSegmentHeader {
        length,
        image_identifier: String::from_utf8_lossy(image_identifier).to_string(),
        image_datetime: String::from_utf8_lossy(image_datetime).to_string(),
        target_identifier: target_identifier.map(|t| String::from_utf8_lossy(&t).to_string()),
        image_title: image_title.map(|t| String::from_utf8_lossy(&t).to_string()),
        security_group,
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
        Ok((_, nitf_header)) => {
            println!("Successfully parsed NITF file: {:?}", nitf_header);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse NITF file: {:?}", e);
            std::process::exit(1);
        }
    }
}