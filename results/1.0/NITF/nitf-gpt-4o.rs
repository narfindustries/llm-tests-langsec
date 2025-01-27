// Due to the complexity and extensiveness of the NITF specification, implementing a full parser using Rust's nom library is a large task. 
// The following is a simplified and partial implementation of such a parser. 
// The full NITF specification contains numerous headers and segments, some of which are optional and contain varied content.
// Therefore, consider this code as a starting scaffold meant for learning and demonstration purposes.

extern crate nom;
use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

// Define structures that represent the parsed data
#[derive(Debug)]
struct NitfHeader {
    file_profile_name: String,
    file_version: String,
    complexity_level: String,
    system_type: String,
    origin_station_id: String,
    start_datetime: String,
    file_title: String,
    file_security_classification: String,
    file_caveats: String,
}

fn parse_string(input: &[u8], size: usize) -> IResult<&[u8], String> {
    map_res(take(size), |s: &[u8]| std::str::from_utf8(s).map(|s| s.trim().to_string()))(input)
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NitfHeader> {
    let (input, file_profile_name) = parse_string(input, 4)?;
    let (input, file_version) = parse_string(input, 5)?;
    let (input, complexity_level) = parse_string(input, 2)?;
    let (input, system_type) = parse_string(input, 1)?;
    let (input, origin_station_id) = parse_string(input, 10)?;
    let (input, start_datetime) = parse_string(input, 14)?;
    let (input, file_title) = parse_string(input, 80)?;
    let (input, file_security_classification) = parse_string(input, 1)?;
    let (input, file_caveats) = parse_string(input, 40)?;

    Ok((input, NitfHeader {
        file_profile_name,
        file_version,
        complexity_level,
        system_type,
        origin_station_id,
        start_datetime,
        file_title,
        file_security_classification,
        file_caveats,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("usage: nitf_parser <input_file>");
        std::process::exit(1);
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_nitf_header(&buffer) {
        Ok((_, header)) => {
            println!("{:?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse NITF header: {:?}", e);
            std::process::exit(1);
        }
    }
}