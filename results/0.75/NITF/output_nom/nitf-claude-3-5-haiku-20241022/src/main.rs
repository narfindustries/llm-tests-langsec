use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take, take_while},
    combinator::{map, opt},
    multi::{count, many0, many_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{tuple, preceded},
    IResult,
};

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
    standard_header_length: u32,
    file_header_length: u32,
    image_segments: Vec<ImageSegment>,
}

#[derive(Debug)]
struct SecurityGroup {
    classification: String,
    security_system: String,
    codewords: Option<String>,
    control_and_handling: Option<String>,
    release_instructions: Option<String>,
    declassification_type: Option<String>,
    declassification_date: Option<String>,
    declassification_exemption: Option<String>,
    downgrade_date: Option<String>,
    downgrade_event: Option<String>,
}

#[derive(Debug)]
struct ImageSegment {
    image_header_length: u32,
    file_partition_number: u32,
    image_title: Option<String>,
    image_security_group: SecurityGroup,
    image_source: Option<String>,
    // Add more image segment fields here
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
        standard_header_length,
        file_header_length
    )) = tuple((
        map(take(2usize), |bytes| String::from_utf8_lossy(bytes).into_owned()),
        map(take(2usize), |bytes| String::from_utf8_lossy(bytes).into_owned()),
        be_u8,
        map(take(4usize), |bytes| String::from_utf8_lossy(bytes).into_owned()),
        map(take(10usize), |bytes| String::from_utf8_lossy(bytes).into_owned()),
        map(take(14usize), |bytes| String::from_utf8_lossy(bytes).into_owned()),
        opt(map(take(80usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned())),
        parse_security_group,
        be_u32,
        be_u32
    ))(input)?;

    let (input, image_segments) = many0(parse_image_segment)(input)?;

    Ok((input, NitfHeader {
        file_profile_name,
        file_version,
        complexity_level,
        standard_type,
        originating_station,
        file_datetime,
        file_title,
        security_group,
        standard_header_length,
        file_header_length,
        image_segments,
    }))
}

fn parse_security_group(input: &[u8]) -> IResult<&[u8], SecurityGroup> {
    let (input, (
        classification,
        security_system,
        codewords,
        control_and_handling,
        release_instructions,
        declassification_type,
        declassification_date,
        declassification_exemption,
        downgrade_date,
        downgrade_event
    )) = tuple((
        map(take(1usize), |bytes| String::from_utf8_lossy(bytes).into_owned()),
        map(take(2usize), |bytes| String::from_utf8_lossy(bytes).into_owned()),
        opt(map(take(11usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned())),
        opt(map(take(2usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned())),
        opt(map(take(20usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned())),
        opt(map(take(2usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned())),
        opt(map(take(8usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned())),
        opt(map(take(4usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned())),
        opt(map(take(8usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned())),
        opt(map(take(6usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned()))
    ))(input)?;

    Ok((input, SecurityGroup {
        classification,
        security_system,
        codewords,
        control_and_handling,
        release_instructions,
        declassification_type,
        declassification_date,
        declassification_exemption,
        downgrade_date,
        downgrade_event,
    }))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, (
        image_header_length,
        file_partition_number,
        image_title,
        image_security_group,
        image_source
    )) = tuple((
        be_u32,
        be_u32,
        opt(map(take(80usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned())),
        parse_security_group,
        opt(map(take(42usize), |bytes: &[u8]| String::from_utf8_lossy(bytes).into_owned()))
    ))(input)?;

    Ok((input, ImageSegment {
        image_header_length,
        file_partition_number,
        image_title,
        image_security_group,
        image_source,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <nitf_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_nitf_header(&buffer) {
        Ok((_, nitf_header)) => {
            println!("Parsed NITF Header: {:?}", nitf_header);
        }
        Err(err) => {
            eprintln!("Failed to parse NITF file: {:?}", err);
            std::process::exit(1);
        }
    }

    Ok(())
}