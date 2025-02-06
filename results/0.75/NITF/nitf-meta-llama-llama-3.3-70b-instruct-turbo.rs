use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take_while_m_n},
    character::complete::{digit1, multispace0},
    combinator::{map, map_res},
    IResult,
};

#[derive(Debug, PartialEq)]
enum SecurityClassification {
    Unclassified,
    Confidential,
    Secret,
    TopSecret,
}

impl SecurityClassification {
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "U" => Ok(SecurityClassification::Unclassified),
            "C" => Ok(SecurityClassification::Confidential),
            "S" => Ok(SecurityClassification::Secret),
            "T" => Ok(SecurityClassification::TopSecret),
            _ => Err(format!("Invalid security classification: {}", s)),
        }
    }
}

#[derive(Debug, PartialEq)]
enum FileType {
    Image,
    Text,
    Table,
    Graphic,
}

impl FileType {
    fn from_str(s: &str) -> Result<Self, String> {
        match s {
            "IMAG" => Ok(FileType::Image),
            "TEXT" => Ok(FileType::Text),
            "TABL" => Ok(FileType::Table),
            "GRAF" => Ok(FileType::Graphic),
            _ => Err(format!("Invalid file type: {}", s)),
        }
    }
}

#[derive(Debug, PartialEq)]
struct FileHeader {
    file_id: String,
    file_type: FileType,
    security_classification: SecurityClassification,
    security_control: String,
    program_id: String,
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, file_id) = take_while_m_n(1, 25, |c| c != b'\x00')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, file_type) = map_res(tag("IMAG"), |s: &[u8]| {
        FileType::from_str(std::str::from_utf8(s).unwrap())
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, security_classification) = map_res(tag("U"), |s: &[u8]| {
        SecurityClassification::from_str(std::str::from_utf8(s).unwrap())
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, security_control) = take_while_m_n(1, 11, |c| c != b'\x00')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, program_id) = take_while_m_n(1, 25, |c| c != b'\x00')(input)?;
    Ok((input, FileHeader {
        file_id: String::from_utf8_lossy(file_id).into_owned(),
        file_type,
        security_classification,
        security_control: String::from_utf8_lossy(security_control).into_owned(),
        program_id: String::from_utf8_lossy(program_id).into_owned(),
    }))
}

#[derive(Debug, PartialEq)]
struct ImageHeader {
    image_id1: String,
    image_id2: String,
    security_classification: SecurityClassification,
    security_code: String,
    comment: String,
    date_time: String,
    time: String,
    day: String,
    location: String,
}

fn parse_image_header(input: &[u8]) -> IResult<&[u8], ImageHeader> {
    let (input, image_id1) = take_while_m_n(1, 25, |c| c != b'\x00')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, image_id2) = take_while_m_n(1, 25, |c| c != b'\x00')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, security_classification) = map_res(tag("U"), |s: &[u8]| {
        SecurityClassification::from_str(std::str::from_utf8(s).unwrap())
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, security_code) = take_while_m_n(1, 11, |c| c != b'\x00')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, comment) = take_while_m_n(1, 79, |c| c != b'\x00')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, date_time) = take_while_m_n(14, 14, |c| c != b'\x00')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, time) = take_while_m_n(6, 6, |c| c != b'\x00')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, day) = take_while_m_n(8, 8, |c| c != b'\x00')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, location) = take_while_m_n(1, 25, |c| c != b'\x00')(input)?;
    Ok((input, ImageHeader {
        image_id1: String::from_utf8_lossy(image_id1).into_owned(),
        image_id2: String::from_utf8_lossy(image_id2).into_owned(),
        security_classification,
        security_code: String::from_utf8_lossy(security_code).into_owned(),
        comment: String::from_utf8_lossy(comment).into_owned(),
        date_time: String::from_utf8_lossy(date_time).into_owned(),
        time: String::from_utf8_lossy(time).into_owned(),
        day: String::from_utf8_lossy(day).into_owned(),
        location: String::from_utf8_lossy(location).into_owned(),
    }))
}

#[derive(Debug, PartialEq)]
struct ImageExtension {
    comment: String,
    date_time: String,
}

fn parse_image_extension(input: &[u8]) -> IResult<&[u8], ImageExtension> {
    let (input, comment) = take_while_m_n(1, 79, |c| c != b'\x00')(input)?;
    let (input, _) = multispace0(input)?;
    let (input, date_time) = take_while_m_n(14, 14, |c| c != b'\x00')(input)?;
    Ok((input, ImageExtension {
        comment: String::from_utf8_lossy(comment).into_owned(),
        date_time: String::from_utf8_lossy(date_time).into_owned(),
    }))
}

#[derive(Debug, PartialEq)]
struct ImageData {
    data: Vec<u8>,
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], ImageData> {
    let (input, data) = take_while_m_n(1, input.len(), |c| c != b'\x00')(input)?;
    Ok((input, ImageData {
        data: data.to_vec(),
    }))
}

#[derive(Debug, PartialEq)]
struct Trailer {
    file_type: FileType,
    file_size: u64,
}

fn parse_trailer(input: &[u8]) -> IResult<&[u8], Trailer> {
    let (input, file_type) = map_res(tag("IMAG"), |s: &[u8]| {
        FileType::from_str(std::str::from_utf8(s).unwrap())
    })(input)?;
    let (input, _) = multispace0(input)?;
    let (input, file_size) = map_res(digit1, |s: &[u8]| {
        std::str::from_utf8(s).unwrap().parse::<u64>().map_err(|_| "Invalid file size")
    })(input)?;
    Ok((input, Trailer {
        file_type,
        file_size,
    }))
}

fn parse_nitf(input: &[u8]) -> IResult<&[u8], (FileHeader, ImageHeader, ImageExtension, ImageData, Trailer)> {
    let (input, file_header) = parse_file_header(input)?;
    let (input, image_header) = parse_image_header(input)?;
    let (input, image_extension) = parse_image_extension(input)?;
    let (input, image_data) = parse_image_data(input)?;
    let (input, trailer) = parse_trailer(input)?;
    Ok((input, (file_header, image_header, image_extension, image_data, trailer)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let mut file = File::open(&args[1]).unwrap();
    let mut input = Vec::new();
    file.read_to_end(&mut input).unwrap();
    match parse_nitf(&input) {
        Ok((_, (file_header, image_header, image_extension, image_data, trailer))) => {
            println!("File Header: {:?}", file_header);
            println!("Image Header: {:?}", image_header);
            println!("Image Extension: {:?}", image_extension);
            println!("Image Data: {:?}", image_data);
            println!("Trailer: {:?}", trailer);
        }
        Err(err) => {
            println!("Error parsing NITF: {:?}", err);
        }
    }
}