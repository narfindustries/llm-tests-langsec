use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{fs, str, env};

// NITF Header Parser
fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFFileHeader> {
    let (input, header) = map_res(take(9usize), str::from_utf8)(input)?;
    let (input, complexity_level) = map_res(take(2usize), str::from_utf8)(input)?;
    let (input, system_type) = map_res(take(4usize), str::from_utf8)(input)?;
    let (input, originating_station_id) = map_res(take(10usize), str::from_utf8)(input)?;
    let (input, file_title) = map_res(take(80usize), str::from_utf8)(input)?;
    let (input, file_date_time) = map_res(take(14usize), str::from_utf8)(input)?; // YYYYMMDDHHMMSS
    let (input, file_security_classification) = map_res(take(1usize), str::from_utf8)(input)?;
    // Additional security fields can be extracted similarly

    Ok((input, NITFFileHeader {
        header: header.to_string(),
        complexity_level: complexity_level.to_string(),
        system_type: system_type.to_string(),
        originating_station_id: originating_station_id.to_string(),
        file_title: file_title.to_string(),
        file_date_time: file_date_time.to_string(),
        file_security_classification: file_security_classification.to_string(),
    }))
}

#[derive(Debug)]
struct NITFFileHeader {
    header: String,
    complexity_level: String,
    system_type: String,
    originating_station_id: String,
    file_title: String,
    file_date_time: String,
    file_security_classification: String,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <NITF file>", args[0]);
        std::process::exit(1);
    }

    let filename = &args[1];
    let data = fs::read(filename).expect("Unable to read file");
    
    match parse_nitf_header(&data) {
        Ok((_, header)) => {
            println!("{:?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse NITF file: {:?}", e);
        }
    }
}
