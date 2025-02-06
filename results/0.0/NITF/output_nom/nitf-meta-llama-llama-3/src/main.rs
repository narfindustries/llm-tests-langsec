use nom::{
    bytes::complete::{tag, take},
    character::complete::multispace1,
    combinator::{map_res, opt},
    multi::{many1},
    number::complete::{be_u32},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};

#[derive(Debug, PartialEq)]
enum Classification {
    Unclassified,
    Confidential,
    Secret,
    TopSecret,
}

#[derive(Debug, PartialEq)]
enum ClassificationSystem {
    US,
    NF,
}

#[derive(Debug, PartialEq)]
enum Compression {
    NoCompression,
    BitCompression,
    Compression,
}

#[derive(Debug, PartialEq)]
enum PixelType {
    Monochrome,
    Color,
    Multispectral,
}

#[derive(Debug, PartialEq)]
enum PixelOrganization {
    BandInterleavedByPixel,
    BandInterleavedByLine,
}

#[derive(Debug, PartialEq)]
struct FileHeader {
    file_header: String,
    file_format_version: String,
    system_type: String,
    file_security_classification: Classification,
    file_title: String,
    file_security_classification_system: ClassificationSystem,
    file_copy_number: u32,
    file_security_control_number: String,
    file_date: String,
    file_time: String,
    file_header_extension: Option<String>,
}

#[derive(Debug, PartialEq)]
struct ImageSegment {
    image_identifier: String,
    image_security_classification: Classification,
    image_security_classification_system: ClassificationSystem,
    image_title: String,
    image_date: String,
    image_time: String,
    image_source: String,
    image_compression: Compression,
    image_pixel_type: PixelType,
    image_pixel_size: u32,
    image_pixel_organization: PixelOrganization,
    image_data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct GraphicSegment {
    graphic_identifier: String,
    graphic_security_classification: Classification,
    graphic_security_classification_system: ClassificationSystem,
    graphic_title: String,
    graphic_date: String,
    graphic_time: String,
    graphic_source: String,
    graphic_data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct TextSegment {
    text_identifier: String,
    text_security_classification: Classification,
    text_security_classification_system: ClassificationSystem,
    text_title: String,
    text_date: String,
    text_time: String,
    text_source: String,
    text_data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct Trailer {
    file_trailer: String,
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, file_header) = tag("NITF")(input)?;
    let (input, _) = take(5u8)(input)?;
    let (input, system_type) = take(25u8)(input)?;
    let (input, file_security_classification) = map_res(take(1u8), |x: &[u8]| match x {
        b"U" => Ok(Classification::Unclassified),
        b"C" => Ok(Classification::Confidential),
        b"S" => Ok(Classification::Secret),
        b"T" => Ok(Classification::TopSecret),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, file_title) = take(80u8)(input)?;
    let (input, file_security_classification_system) = map_res(take(2u8), |x: &[u8]| match x {
        b"US" => Ok(ClassificationSystem::US),
        b"NF" => Ok(ClassificationSystem::NF),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, file_copy_number) = be_u32(input)?;
    let (input, file_security_control_number) = take(20u8)(input)?;
    let (input, file_date) = take(8u8)(input)?;
    let (input, file_time) = take(6u8)(input)?;
    let (input, file_header_extension) = opt(take(25u8))(input)?;
    Ok((
        input,
        FileHeader {
            file_header: String::from_utf8_lossy(file_header).into_owned(),
            file_format_version: String::from("02.10"),
            system_type: String::from_utf8_lossy(system_type).into_owned(),
            file_security_classification,
            file_title: String::from_utf8_lossy(file_title).into_owned(),
            file_security_classification_system,
            file_copy_number,
            file_security_control_number: String::from_utf8_lossy(file_security_control_number)
                .into_owned(),
            file_date: String::from_utf8_lossy(file_date).into_owned(),
            file_time: String::from_utf8_lossy(file_time).into_owned(),
            file_header_extension: file_header_extension.map(|x| String::from_utf8_lossy(x).into_owned()),
        },
    ))
}

fn parse_image_segment(input: &[u8]) -> IResult<&[u8], ImageSegment> {
    let (input, image_identifier) = take(25u8)(input)?;
    let (input, image_security_classification) = map_res(take(1u8), |x: &[u8]| match x {
        b"U" => Ok(Classification::Unclassified),
        b"C" => Ok(Classification::Confidential),
        b"S" => Ok(Classification::Secret),
        b"T" => Ok(Classification::TopSecret),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, image_security_classification_system) = map_res(take(2u8), |x: &[u8]| match x {
        b"US" => Ok(ClassificationSystem::US),
        b"NF" => Ok(ClassificationSystem::NF),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, image_title) = take(80u8)(input)?;
    let (input, image_date) = take(8u8)(input)?;
    let (input, image_time) = take(6u8)(input)?;
    let (input, image_source) = take(42u8)(input)?;
    let (input, image_compression) = map_res(take(2u8), |x: &[u8]| match x {
        b"NC" => Ok(Compression::NoCompression),
        b"BC" => Ok(Compression::BitCompression),
        b"CC" => Ok(Compression::Compression),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, image_pixel_type) = map_res(take(3u8), |x: &[u8]| match x {
        b"MON" => Ok(PixelType::Monochrome),
        b"COL" => Ok(PixelType::Color),
        b"MUL" => Ok(PixelType::Multispectral),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, image_pixel_size) = be_u32(input)?;
    let (input, image_pixel_organization) = map_res(take(3u8), |x: &[u8]| match x {
        b"BIP" => Ok(PixelOrganization::BandInterleavedByPixel),
        b"BIL" => Ok(PixelOrganization::BandInterleavedByLine),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, image_data) = many1(take(1u8))(input)?;
    let image_data: Vec<u8> = image_data.into_iter().map(|x| x[0]).collect();
    Ok((
        input,
        ImageSegment {
            image_identifier: String::from_utf8_lossy(image_identifier).into_owned(),
            image_security_classification,
            image_security_classification_system,
            image_title: String::from_utf8_lossy(image_title).into_owned(),
            image_date: String::from_utf8_lossy(image_date).into_owned(),
            image_time: String::from_utf8_lossy(image_time).into_owned(),
            image_source: String::from_utf8_lossy(image_source).into_owned(),
            image_compression,
            image_pixel_type,
            image_pixel_size,
            image_pixel_organization,
            image_data,
        },
    ))
}

fn parse_graphic_segment(input: &[u8]) -> IResult<&[u8], GraphicSegment> {
    let (input, graphic_identifier) = take(25u8)(input)?;
    let (input, graphic_security_classification) = map_res(take(1u8), |x: &[u8]| match x {
        b"U" => Ok(Classification::Unclassified),
        b"C" => Ok(Classification::Confidential),
        b"S" => Ok(Classification::Secret),
        b"T" => Ok(Classification::TopSecret),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, graphic_security_classification_system) = map_res(take(2u8), |x: &[u8]| match x {
        b"US" => Ok(ClassificationSystem::US),
        b"NF" => Ok(ClassificationSystem::NF),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, graphic_title) = take(80u8)(input)?;
    let (input, graphic_date) = take(8u8)(input)?;
    let (input, graphic_time) = take(6u8)(input)?;
    let (input, graphic_source) = take(42u8)(input)?;
    let (input, graphic_data) = many1(take(1u8))(input)?;
    let graphic_data: Vec<u8> = graphic_data.into_iter().map(|x| x[0]).collect();
    Ok((
        input,
        GraphicSegment {
            graphic_identifier: String::from_utf8_lossy(graphic_identifier).into_owned(),
            graphic_security_classification,
            graphic_security_classification_system,
            graphic_title: String::from_utf8_lossy(graphic_title).into_owned(),
            graphic_date: String::from_utf8_lossy(graphic_date).into_owned(),
            graphic_time: String::from_utf8_lossy(graphic_time).into_owned(),
            graphic_source: String::from_utf8_lossy(graphic_source).into_owned(),
            graphic_data,
        },
    ))
}

fn parse_text_segment(input: &[u8]) -> IResult<&[u8], TextSegment> {
    let (input, text_identifier) = take(25u8)(input)?;
    let (input, text_security_classification) = map_res(take(1u8), |x: &[u8]| match x {
        b"U" => Ok(Classification::Unclassified),
        b"C" => Ok(Classification::Confidential),
        b"S" => Ok(Classification::Secret),
        b"T" => Ok(Classification::TopSecret),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, text_security_classification_system) = map_res(take(2u8), |x: &[u8]| match x {
        b"US" => Ok(ClassificationSystem::US),
        b"NF" => Ok(ClassificationSystem::NF),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    })(input)?;
    let (input, text_title) = take(80u8)(input)?;
    let (input, text_date) = take(8u8)(input)?;
    let (input, text_time) = take(6u8)(input)?;
    let (input, text_source) = take(42u8)(input)?;
    let (input, text_data) = many1(take(1u8))(input)?;
    let text_data: Vec<u8> = text_data.into_iter().map(|x| x[0]).collect();
    Ok((
        input,
        TextSegment {
            text_identifier: String::from_utf8_lossy(text_identifier).into_owned(),
            text_security_classification,
            text_security_classification_system,
            text_title: String::from_utf8_lossy(text_title).into_owned(),
            text_date: String::from_utf8_lossy(text_date).into_owned(),
            text_time: String::from_utf8_lossy(text_time).into_owned(),
            text_source: String::from_utf8_lossy(text_source).into_owned(),
            text_data,
        },
    ))
}

fn parse_trailer(input: &[u8]) -> IResult<&[u8], Trailer> {
    let (input, file_trailer) = tag("NIFF")(input)?;
    Ok((input, Trailer { file_trailer: String::from_utf8_lossy(file_trailer).into_owned() }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let input_file = &args[1];
    let path = Path::new(input_file);
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (input, file_header) = parse_file_header(&input).unwrap();
    let (input, image_segment) = parse_image_segment(input).unwrap();
    let (input, graphic_segment) = parse_graphic_segment(input).unwrap();
    let (input, text_segment) = parse_text_segment(input).unwrap();
    let (_input, trailer) = parse_trailer(input).unwrap();
    println!("File Header: {:?}", file_header);
    println!("Image Segment: {:?}", image_segment);
    println!("Graphic Segment: {:?}", graphic_segment);
    println!("Text Segment: {:?}", text_segment);
    println!("Trailer: {:?}", trailer);
}