use nom::bytes::complete::{take, take_while_m_n};
use nom::combinator::{map, map_res, opt};
use nom::error::ErrorKind;
use nom::multi::many0;
use nom::number::complete::{be_u16, be_u32, be_u64, be_u8};
use nom::sequence::{delimited, preceded, tuple};
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

#[derive(Debug, PartialEq)]
enum NITFContentExtension {
    BASIC,
    EXTENDED,
}

#[derive(Debug, PartialEq)]
struct NITFHeader {
    file_header: String,
    file_version: u8,
    sys_id: String,
    origin_station_id: String,
    file_date: String,
    file_title: String,
    file_security_classification: String,
    file_control_number: String,
    file_copy_number: u8,
    num_headers: u16,
    num_image_segments: u16,
    num_graphics_segments: u16,
    num_text_segments: u16,
    num_data_extension_segments: u16,
    num_reserved_extensions: u16,
    originator_name: String,
    originator_phone: String,
    file_description: String,
    content_extension: Option<NITFContentExtension>,
}

fn parse_nitf_header(input: &[u8]) -> IResult<&[u8], NITFHeader> {
    let (input, file_header) = take(4u8)(input)?;
    let (input, file_version) = be_u8(input)?;
    let (input, sys_id) = take(10u8)(input)?;
    let (input, origin_station_id) = take(10u8)(input)?;
    let (input, file_date) = take(14u8)(input)?;
    let (input, file_title) = take(80u8)(input)?;
    let (input, file_security_classification) = take(1u8)(input)?;
    let (input, file_control_number) = take(25u8)(input)?;
    let (input, file_copy_number) = be_u8(input)?;
    let (input, num_headers) = be_u16(input)?;
    let (input, num_image_segments) = be_u16(input)?;
    let (input, num_graphics_segments) = be_u16(input)?;
    let (input, num_text_segments) = be_u16(input)?;
    let (input, num_data_extension_segments) = be_u16(input)?;
    let (input, num_reserved_extensions) = be_u16(input)?;
    let (input, originator_name) = take(24u8)(input)?;
    let (input, originator_phone) = take(18u8)(input)?;
    let (input, file_description) = take(80u8)(input)?;
    let (input, content_extension) = opt(map_res(take(2u8), |x: &[u8]| {
        match x {
            b"BF" => Ok(NITFContentExtension::BASIC),
            b"EF" => Ok(NITFContentExtension::EXTENDED),
            _ => Err(nom::Err::Error((input, ErrorKind::AlphaNumeric))),
        }
    }))(input)?;
    Ok((
        input,
        NITFHeader {
            file_header: String::from_utf8_lossy(file_header).into_owned(),
            file_version,
            sys_id: String::from_utf8_lossy(sys_id).into_owned(),
            origin_station_id: String::from_utf8_lossy(origin_station_id).into_owned(),
            file_date: String::from_utf8_lossy(file_date).into_owned(),
            file_title: String::from_utf8_lossy(file_title).into_owned(),
            file_security_classification: String::from_utf8_lossy(file_security_classification)
                .into_owned(),
            file_control_number: String::from_utf8_lossy(file_control_number).into_owned(),
            file_copy_number,
            num_headers,
            num_image_segments,
            num_graphics_segments,
            num_text_segments,
            num_data_extension_segments,
            num_reserved_extensions,
            originator_name: String::from_utf8_lossy(originator_name).into_owned(),
            originator_phone: String::from_utf8_lossy(originator_phone).into_owned(),
            file_description: String::from_utf8_lossy(file_description).into_owned(),
            content_extension,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let file_path = Path::new(&args[1]);
    let file = File::open(file_path).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    match parse_nitf_header(&input) {
        Ok((_, header)) => println!("{:?}", header),
        Err(err) => println!("Error: {:?}", err),
    }
}