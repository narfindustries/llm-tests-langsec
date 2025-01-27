use nom::{
    bytes::complete::{take, take_while},
    combinator::{map, map_res, opt},
    error::{Error, ErrorKind},
    multi::{take_while_m_n, many_till},
    number::complete::{be_u8, be_u16, be_u32, be_i8, be_i16, be_i32},
    sequence::{tuple, preceded, terminated},
    IResult,
};
use std::{env, fs, io, str};

const NITF_HEADER_LENGTH: usize = 12;

#[derive(Debug, PartialEq)]
enum NitfSecurityClassification {
    Unclassified,
    Confidential,
    Secret,
    TopSecret,
}

impl NitfSecurityClassification {
    fn parse(input: &[u8]) -> IResult<&[u8], NitfSecurityClassification> {
        match input.get(0) {
            Some(b'U') => Ok((&input[1..], NitfSecurityClassification::Unclassified)),
            Some(b'C') => Ok((&input[1..], NitfSecurityClassification::Confidential)),
            Some(b'S') => Ok((&input[1..], NitfSecurityClassification::Secret)),
            Some(b'T') => Ok((&input[1..], NitfSecurityClassification::TopSecret)),
            _ => Err(nom::Err::Error((input, ErrorKind::AlphaNumeric))),
        }
    }
}

#[derive(Debug)]
struct NitfFileHeader {
    file_header_length: u16,
    file_version: u16,
    file_type: u8,
    file_date: [u8; 6],
    file_title: [u8; 20],
    file_security_classification: NitfSecurityClassification,
    file_control_number: [u8; 20],
}

impl NitfFileHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], NitfFileHeader> {
        let (input, file_header_length) = be_u16(input)?;
        let (input, file_version) = be_u16(input)?;
        let (input, file_type) = be_u8(input)?;
        let (input, file_date) = take(6u8)(input)?;
        let (input, file_title) = take(20u8)(input)?;
        let (input, file_security_classification) = NitfSecurityClassification::parse(input)?;
        let (input, file_control_number) = take(20u8)(input)?;
        Ok((
            input,
            NitfFileHeader {
                file_header_length,
                file_version,
                file_type,
                file_date: file_date.try_into().unwrap(),
                file_title: file_title.try_into().unwrap(),
                file_security_classification,
                file_control_number: file_control_number.try_into().unwrap(),
            },
        ))
    }
}

#[derive(Debug)]
struct NitfImageHeader {
    image_number: [u8; 3],
    security_classification: NitfSecurityClassification,
    image_date: [u8; 6],
    image_title: [u8; 20],
}

impl NitfImageHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], NitfImageHeader> {
        let (input, image_number) = take(3u8)(input)?;
        let (input, security_classification) = NitfSecurityClassification::parse(input)?;
        let (input, image_date) = take(6u8)(input)?;
        let (input, image_title) = take(20u8)(input)?;
        Ok((
            input,
            NitfImageHeader {
                image_number: image_number.try_into().unwrap(),
                security_classification,
                image_date: image_date.try_into().unwrap(),
                image_title: image_title.try_into().unwrap(),
            },
        ))
    }
}

#[derive(Debug)]
struct NitfImageSegment {
    image_header: NitfImageHeader,
    image_data: Vec<u8>,
}

impl NitfImageSegment {
    fn parse(input: &[u8]) -> IResult<&[u8], NitfImageSegment> {
        let (input, image_header) = NitfImageHeader::parse(input)?;
        let (input, image_data_length) = be_u32(input)?;
        let (input, image_data) = take(image_data_length)(input)?;
        Ok((
            input,
            NitfImageSegment {
                image_header,
                image_data: image_data.to_vec(),
            },
        ))
    }
}

#[derive(Debug)]
struct NitfFile {
    file_header: NitfFileHeader,
    image_segments: Vec<NitfImageSegment>,
}

impl NitfFile {
    fn parse(input: &[u8]) -> IResult<&[u8], NitfFile> {
        let (input, file_header) = NitfFileHeader::parse(input)?;
        let mut image_segments = Vec::new();
        let mut input = input;
        while input.len() > 0 {
            let (remaining, image_segment) = NitfImageSegment::parse(input)?;
            image_segments.push(image_segment);
            input = remaining;
        }
        Ok((
            input,
            NitfFile {
                file_header,
                image_segments,
            },
        ))
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let input_file = fs::File::open(&args[1])?;
    let mut reader = io::BufReader::new(input_file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input)?;
    let result = NitfFile::parse(&input);
    match result {
        Ok((remaining, nitf_file)) => {
            println!("Remaining input: {:?}", remaining);
            println!("NITF file: {:?}", nitf_file);
        }
        Err(err) => {
            eprintln!("Error parsing NITF file: {:?}", err);
        }
    }
    Ok(())
}