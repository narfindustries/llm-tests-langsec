use nom::{
    bytes::complete::{tag, take},
    combinator::{flat_map, map, map_res, opt},
    error::ErrorKind,
    multi::{length_data, take_while_m_n},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{Read, Seek, SeekFrom};

#[derive(Debug, PartialEq)]
enum NITFHeaderStandard {
    NITF01,
    NITF02,
    NITF11,
}

impl NITFHeaderStandard {
    fn from_bytes(bytes: &[u8]) -> IResult<&[u8], NITFHeaderStandard> {
        match bytes {
            b"01" => Ok((&bytes[2..], NITFHeaderStandard::NITF01)),
            b"02" => Ok((&bytes[2..], NITFHeaderStandard::NITF02)),
            b"11" => Ok((&bytes[2..], NITFHeaderStandard::NITF11)),
            _ => Err(nom::Err::Error((bytes, ErrorKind::AlphaNumeric))),
        }
    }
}

#[derive(Debug, PartialEq)]
struct NITFHeader {
    standard: NITFHeaderStandard,
    file_version: String,
    file_date_time: String,
    classification: String,
    title: String,
    security_classification: String,
    file_name: String,
    file_part_type: String,
    file_description: String,
    file_segmentation: String,
    file_date_time_generated: String,
}

impl NITFHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], NITFHeader> {
        let (input, _) = tag("NITF")(input)?;
        let (input, standard) = map_Res(be_u8, NITFHeaderStandard::from_bytes)(input)?;
        let (input, file_version) = take(25u8)(input)?;
        let (input, file_date_time) = take(20u8)(input)?;
        let (input, classification) = take(1u8)(input)?;
        let (input, title) = take(80u8)(input)?;
        let (input, security_classification) = take(1u8)(input)?;
        let (input, file_name) = take(24u8)(input)?;
        let (input, file_part_type) = take(2u8)(input)?;
        let (input, file_description) = take(25u8)(input)?;
        let (input, file_segmentation) = take(2u8)(input)?;
        let (input, file_date_time_generated) = take(20u8)(input)?;

        Ok((
            input,
            NITFHeader {
                standard,
                file_version: String::from_utf8_lossy(file_version).into_owned(),
                file_date_time: String::from_utf8_lossy(file_date_time).into_owned(),
                classification: String::from_utf8_lossy(classification).into_owned(),
                title: String::from_utf8_lossy(title).into_owned(),
                security_classification: String::from_utf8_lossy(security_classification)
                    .into_owned(),
                file_name: String::from_utf8_lossy(file_name).into_owned(),
                file_part_type: String::from_utf8_lossy(file_part_type).into_owned(),
                file_description: String::from_utf8_lossy(file_description).into_owned(),
                file_segmentation: String::from_utf8_lossy(file_segmentation).into_owned(),
                file_date_time_generated: String::from_utf8_lossy(file_date_time_generated)
                    .into_owned(),
            },
        ))
    }
}

#[derive(Debug, PartialEq)]
struct ImageSegment {
    header_length: u16,
    image_id: String,
    image_date_time: String,
    image_compression: String,
    source_id: String,
    source_date_time: String,
}

impl ImageSegment {
    fn parse(input: &[u8]) -> IResult<&[u8], ImageSegment> {
        let (input, header_length) = be_u16(input)?;
        let (input, _) = take(header_length - 12)(input)?;
        let (input, image_id) = take(25u8)(input)?;
        let (input, image_date_time) = take(20u8)(input)?;
        let (input, image_compression) = take(2u8)(input)?;
        let (input, source_id) = take(25u8)(input)?;
        let (input, source_date_time) = take(20u8)(input)?;

        Ok((
            input,
            ImageSegment {
                header_length,
                image_id: String::from_utf8_lossy(image_id).into_owned(),
                image_date_time: String::from_utf8_lossy(image_date_time).into_owned(),
                image_compression: String::from_utf8_lossy(image_compression).into_owned(),
                source_id: String::from_utf8_lossy(source_id).into_owned(),
                source_date_time: String::from_utf8_lossy(source_date_time).into_owned(),
            },
        ))
    }
}

#[derive(Debug, PartialEq)]
struct TextSegment {
    header_length: u16,
    text_id: String,
    text_date_time: String,
    text_compression: String,
}

impl TextSegment {
    fn parse(input: &[u8]) -> IResult<&[u8], TextSegment> {
        let (input, header_length) = be_u16(input)?;
        let (input, _) = take(header_length - 9)(input)?;
        let (input, text_id) = take(25u8)(input)?;
        let (input, text_date_time) = take(20u8)(input)?;
        let (input, text_compression) = take(2u8)(input)?;

        Ok((
            input,
            TextSegment {
                header_length,
                text_id: String::from_utf8_lossy(text_id).into_owned(),
                text_date_time: String::from_utf8_lossy(text_date_time).into_owned(),
                text_compression: String::from_utf8_lossy(text_compression).into_owned(),
            },
        ))
    }
}

#[derive(Debug, PartialEq)]
struct GraphicsSegment {
    header_length: u16,
    graphics_id: String,
    graphics_date_time: String,
    graphics_compression: String,
}

impl GraphicsSegment {
    fn parse(input: &[u8]) -> IResult<&[u8], GraphicsSegment> {
        let (input, header_length) = be_u16(input)?;
        let (input, _) = take(header_length - 9)(input)?;
        let (input, graphics_id) = take(25u8)(input)?;
        let (input, graphics_date_time) = take(20u8)(input)?;
        let (input, graphics_compression) = take(2u8)(input)?;

        Ok((
            input,
            GraphicsSegment {
                header_length,
                graphics_id: String::from_utf8_lossy(graphics_id).into_owned(),
                graphics_date_time: String::from_utf8_lossy(graphics_date_time).into_owned(),
                graphics_compression: String::from_utf8_lossy(graphics_compression).into_owned(),
            },
        ))
    }
}

#[derive(Debug, PartialEq)]
enum NITFSegment {
    Image(ImageSegment),
    Text(TextSegment),
    Graphics(GraphicsSegment),
}

impl NITFSegment {
    fn parse(input: &[u8]) -> IResult<&[u8], NITFSegment> {
        let (input, segment_type) = take(2u8)(input)?;
        match segment_type {
            b"IM" => {
                let (input, image_segment) = ImageSegment::parse(input)?;
                Ok((input, NITFSegment::Image(image_segment)))
            }
            b"TE" => {
                let (input, text_segment) = TextSegment::parse(input)?;
                Ok((input, NITFSegment::Text(text_segment)))
            }
            b"GR" => {
                let (input, graphics_segment) = GraphicsSegment::parse(input)?;
                Ok((input, NITFSegment::Graphics(graphics_segment)))
            }
            _ => Err(nom::Err::Error((input, ErrorKind::AlphaNumeric))),
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }

    let mut file = File::open(&args[1]).expect("Unable to open file!");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file!");

    match NITFHeader::parse(&buffer) {
        Ok((remaining, header)) => {
            println!("NITF Header: {:?}", header);
            let mut input = remaining;
            loop {
                match NITFSegment::parse(input) {
                    Ok((remaining, segment)) => {
                        println!("NITF Segment: {:?}", segment);
                        input = remaining;
                    }
                    Err(_) => break,
                }
            }
        }
        Err(e) => panic!("Error parsing NITF header: {:?}", e),
    }
}