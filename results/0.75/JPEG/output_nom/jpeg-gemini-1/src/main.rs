use std::env;
use std::fs;
use std::path::Path;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    number::complete::{be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};

#[derive(Debug)]
struct JpegHeader {
    soi: [u8; 2],
    segments: Vec<JpegSegment>,
    eoi: [u8; 2],
}

#[derive(Debug)]
enum JpegSegment {
    StartOfFrame(StartOfFrame),
    DefineQuantizationTable(DefineQuantizationTable),
    DefineHuffmanTable(DefineHuffmanTable),
    // Add other segment types as needed...
    Unknown(Vec<u8>),
}

#[derive(Debug)]
struct StartOfFrame {
    marker: [u8; 2],
    length: u16,
    precision: u8,
    height: u16,
    width: u16,
    // Add other SOF fields as needed...
}

#[derive(Debug)]
struct DefineQuantizationTable {
    marker: [u8; 2],
    length: u16,
    // Add other DQT fields as needed...
}

#[derive(Debug)]
struct DefineHuffmanTable {
    marker: [u8; 2],
    length: u16,
    // Add other DHT fields as needed...

}


fn jpeg_header(input: &[u8]) -> IResult<&[u8], JpegHeader> {
    let (input, soi) = tag(b"\xFF\xD8")(input)?;
    let (input, segments) = many0(jpeg_segment)(input)?;
    let (input, eoi) = tag(b"\xFF\xD9")(input)?;
    Ok((input, JpegHeader { soi, segments, eoi }))
}

fn jpeg_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = recognize(tuple((tag(b"\xFF"), take(1_usize))))(input)?;
    let marker_byte = marker[1];

    match marker_byte {
        0xC0 => {
            let (input, segment) = start_of_frame(input)?;
            Ok((input, JpegSegment::StartOfFrame(segment)))
        },
        0xDB => {
            let (input, segment) = define_quantization_table(input)?;
            Ok((input, JpegSegment::DefineQuantizationTable(segment)))
        },
        0xC4 => {
            let (input, segment) = define_huffman_table(input)?;
            Ok((input, JpegSegment::DefineHuffmanTable(segment)))
        },
        _ => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length as usize - 2)(input)?;
            Ok((input, JpegSegment::Unknown(data.to_vec())))
        }
    }
}

fn start_of_frame(input: &[u8]) -> IResult<&[u8], StartOfFrame> {
    let (input, marker) = tag(b"\xFF\xC0")(input)?;
    let (input, length) = be_u16(input)?;
    let (input, precision) = take(1_usize)(input)?;
    let (input, height) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    Ok((input, StartOfFrame { marker: [0xFF, 0xC0], length, precision: precision[0], height, width }))
}

fn define_quantization_table(input: &[u8]) -> IResult<&[u8], DefineQuantizationTable> {
    let (input, marker) = tag(b"\xFF\xDB")(input)?;
    let (input, length) = be_u16(input)?;
    Ok((input, DefineQuantizationTable { marker: [0xFF, 0xDB], length }))
}

fn define_huffman_table(input: &[u8]) -> IResult<&[u8], DefineHuffmanTable> {
    let (input, marker) = tag(b"\xFF\xC4")(input)?;
    let (input, length) = be_u16(input)?;
    Ok((input, DefineHuffmanTable { marker: [0xFF, 0xC4], length }))
}


use nom::multi::many0;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <JPEG_FILE>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let data = match fs::read(path) {
        Ok(data) => data,
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };

    match jpeg_header(&data) {
        Ok((_, header)) => println!("{:#?}", header),
        Err(err) => eprintln!("Error parsing JPEG: {:?}", err),
    }
}
