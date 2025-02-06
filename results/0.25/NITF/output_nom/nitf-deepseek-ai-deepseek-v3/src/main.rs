use nom::{
    bytes::complete::take,
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct NITFHeader {
    file_profile_name: String,
    file_version: String,
    complexity_level: String,
    system_type: String,
    origin_station_id: String,
    file_date_time: String,
    file_title: String,
    classification: String,
    classification_system: String,
    codewords: String,
    control_and_handling: String,
    releasing_instructions: String,
    declassification_type: String,
    declassification_date: String,
    declassification_exemption: String,
    downgrade: String,
    downgrade_date: String,
    classification_text: String,
    classification_authority_type: String,
    classification_authority: String,
    classification_reason: String,
    source_date: String,
    control_number: String,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, file_profile_name) = take(4usize)(input)?;
    let (input, file_version) = take(2usize)(input)?;
    let (input, complexity_level) = take(2usize)(input)?;
    let (input, system_type) = take(4usize)(input)?;
    let (input, origin_station_id) = take(10usize)(input)?;
    let (input, file_date_time) = take(14usize)(input)?;
    let (input, file_title) = take(80usize)(input)?;
    let (input, classification) = take(1usize)(input)?;
    let (input, classification_system) = take(2usize)(input)?;
    let (input, codewords) = take(11usize)(input)?;
    let (input, control_and_handling) = take(2usize)(input)?;
    let (input, releasing_instructions) = take(20usize)(input)?;
    let (input, declassification_type) = take(2usize)(input)?;
    let (input, declassification_date) = take(8usize)(input)?;
    let (input, declassification_exemption) = take(4usize)(input)?;
    let (input, downgrade) = take(1usize)(input)?;
    let (input, downgrade_date) = take(8usize)(input)?;
    let (input, classification_text) = take(43usize)(input)?;
    let (input, classification_authority_type) = take(1usize)(input)?;
    let (input, classification_authority) = take(40usize)(input)?;
    let (input, classification_reason) = take(1usize)(input)?;
    let (input, source_date) = take(8usize)(input)?;
    let (input, control_number) = take(15usize)(input)?;

    Ok((input, NITFHeader {
        file_profile_name: String::from_utf8_lossy(file_profile_name).to_string(),
        file_version: String::from_utf8_lossy(file_version).to_string(),
        complexity_level: String::from_utf8_lossy(complexity_level).to_string(),
        system_type: String::from_utf8_lossy(system_type).to_string(),
        origin_station_id: String::from_utf8_lossy(origin_station_id).to_string(),
        file_date_time: String::from_utf8_lossy(file_date_time).to_string(),
        file_title: String::from_utf8_lossy(file_title).to_string(),
        classification: String::from_utf8_lossy(classification).to_string(),
        classification_system: String::from_utf8_lossy(classification_system).to_string(),
        codewords: String::from_utf8_lossy(codewords).to_string(),
        control_and_handling: String::from_utf8_lossy(control_and_handling).to_string(),
        releasing_instructions: String::from_utf8_lossy(releasing_instructions).to_string(),
        declassification_type: String::from_utf8_lossy(declassification_type).to_string(),
        declassification_date: String::from_utf8_lossy(declassification_date).to_string(),
        declassification_exemption: String::from_utf8_lossy(declassification_exemption).to_string(),
        downgrade: String::from_utf8_lossy(downgrade).to_string(),
        downgrade_date: String::from_utf8_lossy(downgrade_date).to_string(),
        classification_text: String::from_utf8_lossy(classification_text).to_string(),
        classification_authority_type: String::from_utf8_lossy(classification_authority_type).to_string(),
        classification_authority: String::from_utf8_lossy(classification_authority).to_string(),
        classification_reason: String::from_utf8_lossy(classification_reason).to_string(),
        source_date: String::from_utf8_lossy(source_date).to_string(),
        control_number: String::from_utf8_lossy(control_number).to_string(),
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
        Ok((_, header)) => println!("{:?}", header),
        Err(e) => eprintln!("Failed to parse NITF header: {:?}", e),
    }
}