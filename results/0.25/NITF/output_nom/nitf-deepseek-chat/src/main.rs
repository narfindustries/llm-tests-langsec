use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32, be_u64},
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
    complexity_level: String,
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
    classification_system: String,
    codewords: String,
    control_and_handling: String,
    release_instructions: String,
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

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, (file_profile_name, file_version, complexity_level, standard_type, originating_station_id, file_date_and_time, file_title, file_security, file_copy_number, file_number_of_extension_segments, file_length, header_length)) = tuple((
        take(4usize),
        take(2usize),
        take(2usize),
        take(4usize),
        take(10usize),
        take(14usize),
        take(80usize),
        parse_file_security,
        take(5usize),
        be_u16,
        be_u64,
        be_u32,
    ))(input)?;

    Ok((input, NITFHeader {
        file_profile_name: String::from_utf8_lossy(file_profile_name).to_string(),
        file_version: String::from_utf8_lossy(file_version).to_string(),
        complexity_level: String::from_utf8_lossy(complexity_level).to_string(),
        standard_type: String::from_utf8_lossy(standard_type).to_string(),
        originating_station_id: String::from_utf8_lossy(originating_station_id).to_string(),
        file_date_and_time: String::from_utf8_lossy(file_date_and_time).to_string(),
        file_title: String::from_utf8_lossy(file_title).to_string(),
        file_security,
        file_copy_number: String::from_utf8_lossy(file_copy_number).to_string(),
        file_number_of_extension_segments,
        file_length,
        header_length,
    }))
}

fn parse_file_security(input: &[u8]) -> IResult<&[u8], FileSecurity> {
    let (input, (classification_system, codewords, control_and_handling, release_instructions, declassification_type, declassification_date, declassification_exemption, downgrade, downgrade_date, classification_text, classification_authority_type, classification_authority, classification_reason, security_source_date, security_control_number)) = tuple((
        take(2usize),
        take(11usize),
        take(2usize),
        take(20usize),
        take(2usize),
        take(8usize),
        take(4usize),
        take(1usize),
        take(8usize),
        take(43usize),
        take(1usize),
        take(40usize),
        take(1usize),
        take(8usize),
        take(15usize),
    ))(input)?;

    Ok((input, FileSecurity {
        classification_system: String::from_utf8_lossy(classification_system).to_string(),
        codewords: String::from_utf8_lossy(codewords).to_string(),
        control_and_handling: String::from_utf8_lossy(control_and_handling).to_string(),
        release_instructions: String::from_utf8_lossy(release_instructions).to_string(),
        declassification_type: String::from_utf8_lossy(declassification_type).to_string(),
        declassification_date: String::from_utf8_lossy(declassification_date).to_string(),
        declassification_exemption: String::from_utf8_lossy(declassification_exemption).to_string(),
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

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <NITF file>", args[0]);
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