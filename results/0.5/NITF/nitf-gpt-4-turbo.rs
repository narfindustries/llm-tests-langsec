use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map_res, verify},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read, str};

/// Parse a fixed length ASCII field
fn parse_fixed_length_ascii(input: &[u8], length: usize) -> IResult<&[u8], &str> {
    map_res(take(length), str::from_utf8)(input)
}

/// Parse a numeric field with a fixed length
fn parse_numeric(input: &[u8], length: usize) -> IResult<&[u8], u32> {
    map_res(
        map_res(take(length), str::from_utf8),
        |s: &str| s.parse::<u32>(),
    )(input)
}

/// Parse the NITF File Header
fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, (file_type, file_version, complex_flag, system_type, origin_station_id, file_date_time, file_title, file_security_class, copy_num, num_img_segments)) = tuple((
        parse_fixed_length_ascii(9usize),
        parse_fixed_length_ascii(2usize),
        parse_fixed_length_ascii(1usize),
        parse_fixed_length_ascii(4usize),
        parse_fixed_length_ascii(10usize),
        parse_fixed_length_ascii(14usize),
        parse_fixed_length_ascii(80usize),
        parse_fixed_length_ascii(1usize),
        parse_numeric(5usize),
        parse_numeric(3usize),
    ))(input)?;

    Ok((
        input,
        FileHeader {
            file_type: file_type.to_string(),
            file_version: file_version.to_string(),
            complex_flag: complex_flag.to_string(),
            system_type: system_type.to_string(),
            origin_station_id: origin_station_id.to_string(),
            file_date_time: file_date_time.to_string(),
            file_title: file_title.to_string(),
            file_security_class: file_security_class.to_string(),
            copy_num,
            num_img_segments,
        },
    ))
}

#[derive(Debug)]
struct FileHeader {
    file_type: String,
    file_version: String,
    complex_flag: String,
    system_type: String,
    origin_station_id: String,
    file_date_time: String,
    file_title: String,
    file_security_class: String,
    copy_num: u32,
    num_img_segments: u32,
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <NITF_FILE>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_file_header(&buffer) {
        Ok((_, header)) => {
            println!("Parsed NITF File Header: {:?}", header);
        }
        Err(e) => {
            eprintln!("Failed to parse NITF File Header: {:?}", e);
        }
    }
}