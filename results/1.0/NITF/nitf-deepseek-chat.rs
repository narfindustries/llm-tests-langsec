use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u8, be_u16, be_u32, be_u64},
    sequence::tuple,
    IResult,
    multi::length_data,
    combinator::map,
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
    file_copy_number: String,
    file_number_of_extension_segments: u16,
    file_length: u64,
    header_length: u32,
}

#[derive(Debug)]
struct FileSecurity {
    classification: String,
    codewords: String,
    control_and_handling: String,
    releaseability: String,
    declassification_type: String,
    declassification_date: String,
    declassification_exemption: String,
    downgrade: String,
    downgrade_date: String,
    classification_text: String,
    classification_authority_type: String,
    classification_authority: String,
    classification_reason: String,
    security_source_date: String,
    security_control_number: String,
}

fn parse_file_security(input: &[u8]) -> IResult<&[u8], FileSecurity> {
    let (input, classification) = map(take(1usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, codewords) = map(take(40usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, control_and_handling) = map(take(40usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, releaseability) = map(take(20usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, declassification_type) = map(take(2usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, declassification_date) = map(take(8usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, declassification_exemption) = map(take(4usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, downgrade) = map(take(1usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, downgrade_date) = map(take(8usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, classification_text) = map(take(43usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, classification_authority_type) = map(take(1usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, classification_authority) = map(take(40usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, classification_reason) = map(take(1usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, security_source_date) = map(take(8usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, security_control_number) = map(take(15usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;

    Ok((input, FileSecurity {
        classification,
        codewords,
        control_and_handling,
        releaseability,
        declassification_type,
        declassification_date,
        declassification_exemption,
        downgrade,
        downgrade_date,
        classification_text,
        classification_authority_type,
        classification_authority,
        classification_reason,
        security_source_date,
        security_control_number,
    }))
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, file_profile_name) = map(take(4usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, file_version) = map(take(5usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, complexity_level) = be_u8(input)?;
    let (input, standard_type) = map(take(2usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, originating_station_id) = map(take(10usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, file_date_and_time) = map(take(14usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, file_title) = map(take(80usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, file_security) = parse_file_security(input)?;
    let (input, file_copy_number) = map(take(5usize), |v: &[u8]| String::from_utf8_lossy(v).into_owned())(input)?;
    let (input, file_number_of_extension_segments) = be_u16(input)?;
    let (input, file_length) = be_u64(input)?;
    let (input, header_length) = be_u32(input)?;

    Ok((input, NITFHeader {
        file_profile_name,
        file_version,
        complexity_level,
        standard_type,
        originating_station_id,
        file_date_and_time,
        file_title,
        file_security,
        file_copy_number,
        file_number_of_extension_segments,
        file_length,
        header_length,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
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