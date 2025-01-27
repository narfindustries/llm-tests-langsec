use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    error::ErrorKind,
    number::complete::be_u16,
    sequence::{preceded, tuple},
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
    DQT(DQT),
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
    // Add other fields as needed...
}


#[derive(Debug)]
struct DQT {
    marker: [u8;2],
    length: u16,
    quantization_tables: Vec<u8>,
}

fn jpeg_header(input: &[u8]) -> IResult<&[u8], JpegHeader> {
    let (input, soi) = tag(b"\xFF\xD8")(input)?;
    let (input, segments) = many0(jpeg_segment)(input)?;
    let (input, eoi) = tag(b"\xFF\xD9")(input)?;
    Ok((input, JpegHeader { soi, segments, eoi }))
}


fn jpeg_segment(input: &[u8]) -> IResult<&[u8], JpegSegment> {
    let (input, marker) = preceded(tag(b"\xFF"), take(1usize))(input)?;
    match marker[0] {
        0xC0 => {
            map(start_of_frame, JpegSegment::StartOfFrame)(input)
        }
        0xDB => {
            map(dqt_segment, JpegSegment::DQT)(input)
        }
        _ => {
            let (input, length) = be_u16(input)?;
            let (input, data) = take(length as usize -2)(input)?;
            Ok((input, JpegSegment::Unknown(data.to_vec())))
        }
    }
}

fn start_of_frame(input: &[u8]) -> IResult<&[u8], StartOfFrame> {
    let (input, (marker, length, precision, height, width)) = tuple((
        tag(b"\xFF\xC0"),
        be_u16,
        take(1usize),
        be_u16,
        be_u16,
    ))(input)?;
    Ok((
        input,
        StartOfFrame {
            marker: marker.try_into().unwrap(),
            length,
            precision: precision[0],
            height,
            width,
        },
    ))
}

fn dqt_segment(input: &[u8]) -> IResult<&[u8], DQT> {
    let (input, (marker, length)) = tuple((tag(b"\xFF\xDB"), be_u16))(input)?;
    let (input, data) = take((length as usize) - 2)(input)?;
    Ok((input, DQT { marker: marker.try_into().unwrap(), length, quantization_tables: data.to_vec()}))
}


fn many0<I, O, E, F>(f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Clone + std::fmt::Debug,
    F: FnMut(I) -> IResult<I, O, E>,
    E: nom::error::ParseError<I>,
{
    nom::multi::many0(f)
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <jpeg_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match jpeg_header(&buffer) {
        Ok((_, header)) => println!("JPEG Header: {:?}", header),
        Err(err) => println!("Error parsing JPEG: {:?}", err),
    }
}
