use nom::{
    bytes::complete::{tag, take, take_while},
    combinator::{map, opt},
    multi::{count, many0, many_m_n},
    number::complete::{be_u8, be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NitfHeader {
    file_profile_name: String,
    file_version: String,
    complexity_level: u8,
    standard_type: String,
    originating_station: String,
    file_datetime: String,
    file_title: Option<String>,
    security_group: SecurityGroup,
    extended_headers: Vec<ExtendedHeader>,
    image_segments: Vec<ImageSegment>,
    graphic_segments: Vec<GraphicSegment>,
    text_segments: Vec<TextSegment>,
    data_extension_segments: Vec<DataExtensionSegment>,
    reserved_extension_segments: Vec<ReservedExtensionSegment>,
}

#[derive(Debug)]
struct SecurityGroup {
    classification: String,
    security_system: String,
    codewords: Option<String>,
    control_and_handling: Option<String>,
    release_instructions: Option<String>,
    declassification_date: Option<String>,
    declassification_exemption: Option<String>,
    downgrade_date: Option<String>,
    downgrade_event: Option<String>,
}

#[derive(Debug)]
struct ExtendedHeader {
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
    security_group: SecurityGroup,
    encryption: Option<String>,
    image_source: Option<String>,
    significant_rows: u32,
    significant_columns: u32,
    pixel_type: String,
    image_representation: String,
    image_category: String,
}

#[derive(Debug)]
struct GraphicSegment {
    header: GraphicSegmentHeader,
    graphic_data: Vec<u8>,
}

#[derive(Debug)]
struct GraphicSegmentHeader {
    length: u32,
    graphic_identifier: String,
    graphic_name: Option<String>,
    security_group: SecurityGroup,
}

#[derive(Debug)]
struct TextSegment {
    header: TextSegmentHeader,
    text_data: Vec<u8>,
}

#[derive(Debug)]
struct TextSegmentHeader {
    length: u32,
    text_identifier: String,
    text_attachment_level: Option<u8>,
    security_group: SecurityGroup,
}

#[derive(Debug)]
struct DataExtensionSegment {
    header: DataExtensionSegmentHeader,
    data: Vec<u8>,
}

#[derive(Debug)]
struct DataExtensionSegmentHeader {
    length: u32,
    des_identifier: String,
    security_group: SecurityGroup,
}

#[derive(Debug)]
struct ReservedExtensionSegment {
    header: ReservedExtensionSegmentHeader,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ReservedExtensionSegmentHeader {
    length: u32,
    res_identifier: String,
    security_group: SecurityGroup,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, (
        file_profile_name,
        file_version,
        complexity_level,
        standard_type,
        originating_station,
        file_datetime,
        file_title,
        security_group,
        extended_headers,
        image_segments,
        graphic_segments,
        text_segments,
        data_extension_segments,
        reserved_extension_segments
    )) = tuple((
        parse_fixed_string(9),
        parse_fixed_string(2),
        be_u8,
        parse_fixed_string(10),
        parse_fixed_string(10),
        parse_fixed_string(14),
        opt(parse_fixed_string(80)),
        parse_security_group,
        many0(parse_extended_header),
        many0(parse_image_segment),
        many0(parse_graphic_segment),
        many0(parse_text_segment),
        many0(parse_data_extension_segment),
        many0(parse_reserved_extension_segment)
    ))(input)?;

    Ok((input, NitfHeader {
        file_profile_name,
        file_version,
        complexity_level,
        standard_type,
        originating_station,
        file_datetime,
        file_title,
        security_group,
        extended_headers,
        image_segments,
        graphic_segments,
        text_segments,
        data_extension_segments,
        reserved_extension_segments,
    }))
}

// Implement other parsing functions for each struct...
// (Omitted for brevity, but would follow similar patterns)

fn parse_fixed_string(length: usize) -> impl Fn(&[u8]) -> IResult<&[u8], String> {
    move |input| {
        let (input, bytes) = take(length)(input)?;
        Ok((input, String::from_utf8_lossy(bytes).into_owned()))
    }
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
        Ok((_, nitf)) => {
            println!("Parsed NITF: {:?}", nitf);
            Ok(())
        }
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1)
        }
    }
}

// Additional parsing functions would be implemented here for each struct
// with detailed parsing logic for NITF specification