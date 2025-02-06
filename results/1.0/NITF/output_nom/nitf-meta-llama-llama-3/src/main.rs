use nom::{
    IResult, bytes::complete::{take, take_while_m_n},
    combinator::{map_res, verify},
    number::complete::{be_u8, be_u16, be_u32, be_i32},
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

// NITF File Header
#[derive(Debug, PartialEq)]
struct FileHeader {
    sy: String,
    sfs: u8,
    fdt: String,
    ttl: String,
}

// NITF Image Header
#[derive(Debug, PartialEq)]
struct ImageHeader {
    id: String,
    cclass: String,
    rdt: String,
    tgtid: String,
}

// NITF Image Segment Header
#[derive(Debug, PartialEq)]
struct ImageSegmentHeader {
    ishid: String,
    ishc: String,
    ishd: String,
}

// NITF Data Extension Segment
#[derive(Debug, PartialEq)]
struct DataExtensionSegment {
    desid: String,
    dest: u8,
    desv: u8,
}

// NITF Text Segment
#[derive(Debug, PartialEq)]
struct TextSegment {
    tsid: String,
    tst: u8,
    tsv: u8,
}

// NITF Image Data
#[derive(Debug, PartialEq)]
struct ImageData {
    idat: Vec<u8>,
}

fn parse_file_header(input: &[u8]) -> IResult<&[u8], FileHeader> {
    let (input, sy) = take(4u8)(input)?;
    let (input, sfs) = be_u8(input)?;
    let (input, fdt) = take(8u8)(input)?;
    let (input, ttl) = take_while_m_n(1, 80, |c| c != 0)(input)?;
    Ok((input, FileHeader {
        sy: std::str::from_utf8(sy).unwrap().to_string(),
        sfs,
        fdt: std::str::from_utf8(fdt).unwrap().to_string(),
        ttl: std::str::from_utf8(ttl).unwrap().to_string(),
    }))
}

fn parse_image_header(input: &[u8]) -> IResult<&[u8], ImageHeader> {
    let (input, id) = take(25u8)(input)?;
    let (input, cclass) = take(1u8)(input)?;
    let (input, rdt) = take(8u8)(input)?;
    let (input, tgtid) = take(20u8)(input)?;
    Ok((input, ImageHeader {
        id: std::str::from_utf8(id).unwrap().to_string(),
        cclass: std::str::from_utf8(cclass).unwrap().to_string(),
        rdt: std::str::from_utf8(rdt).unwrap().to_string(),
        tgtid: std::str::from_utf8(tgtid).unwrap().to_string(),
    }))
}

fn parse_image_segment_header(input: &[u8]) -> IResult<&[u8], ImageSegmentHeader> {
    let (input, ishid) = take(25u8)(input)?;
    let (input, ishc) = take(1u8)(input)?;
    let (input, ishd) = take(8u8)(input)?;
    Ok((input, ImageSegmentHeader {
        ishid: std::str::from_utf8(ishid).unwrap().to_string(),
        ishc: std::str::from_utf8(ishc).unwrap().to_string(),
        ishd: std::str::from_utf8(ishd).unwrap().to_string(),
    }))
}

fn parse_data_extension_segment(input: &[u8]) -> IResult<&[u8], DataExtensionSegment> {
    let (input, desid) = take(25u8)(input)?;
    let (input, dest) = be_u8(input)?;
    let (input, desv) = be_u8(input)?;
    Ok((input, DataExtensionSegment {
        desid: std::str::from_utf8(desid).unwrap().to_string(),
        dest,
        desv,
    }))
}

fn parse_text_segment(input: &[u8]) -> IResult<&[u8], TextSegment> {
    let (input, tsid) = take(25u8)(input)?;
    let (input, tst) = be_u8(input)?;
    let (input, tsv) = be_u8(input)?;
    Ok((input, TextSegment {
        tsid: std::str::from_utf8(tsid).unwrap().to_string(),
        tst,
        tsv,
    }))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], ImageData> {
    let (input, idat_len) = be_u32(input)?;
    let (input, idat) = take(idat_len)(input)?;
    Ok((input, ImageData { idat: idat.to_vec() }))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }

    let input_file = &args[1];
    let path = Path::new(input_file);
    let mut file = File::open(path).unwrap();
    let mut input = Vec::new();
    file.read_to_end(&mut input).unwrap();

    let (input, file_header) = parse_file_header(&input).unwrap();
    println!("File Header: {:?}", file_header);

    let (input, image_header) = parse_image_header(input).unwrap();
    println!("Image Header: {:?}", image_header);

    let (input, image_segment_header) = parse_image_segment_header(input).unwrap();
    println!("Image Segment Header: {:?}", image_segment_header);

    let (input, data_extension_segment) = parse_data_extension_segment(input).unwrap();
    println!("Data Extension Segment: {:?}", data_extension_segment);

    let (input, text_segment) = parse_text_segment(input).unwrap();
    println!("Text Segment: {:?}", text_segment);

    let (input, image_data) = parse_image_data(input).unwrap();
    println!("Image Data: {:?}", image_data);
}