extern crate nom;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt, opt},
    multi::{length_data, many0, many1},
    number::complete::{be_u16, be_u32},
    sequence::{delimited, preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

#[derive(Debug)]
struct DicomFile {
    preamble: Vec<u8>,
    dicom_prefix: Vec<u8>,
    data_set: DataSet,
}

#[derive(Debug)]
struct DataSet {
    elements: Vec<DataElement>,
}

#[derive(Debug)]
struct DataElement {
    tag: (u16, u16),
    vr: Option<(u8, u8, u8)>,
    length: Option<u32>,
    value: Option<Vec<u8>>,
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], DicomFile> {
    let (input, preamble) = take(128)(input)?;
    let (input, dicom_prefix) = tag(b"DICM")(input)?;
    let (input, data_set) = parse_data_set(input)?;
    Ok((input, DicomFile { preamble: preamble.to_vec(), dicom_prefix: dicom_prefix.to_vec(), data_set }))
}

fn parse_data_set(input: &[u8]) -> IResult<&[u8], DataSet> {
    let (input, elements) = many0(parse_data_element)(input)?;
    Ok((input, DataSet { elements }))
}

fn parse_data_element(input: &[u8]) -> IResult<&[u8], DataElement> {
    let (input, tag) = map(tuple((be_u16, be_u16)), |(g, e)| (g, e))(input)?;
    let (input, vr) = opt(preceded(tag(b"VR="), take(3u8)))(input)?;
    let (input, length) = opt(preceded(tag(b"VL="), be_u32))(input)?;
    let (input, value) = opt(map_opt(length_data(be_u32), |x| x))(input)?;
    Ok((input, DataElement { tag, vr: vr.map(|x| (x[0], x[1], x[2])), length, value }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let path = Path::new(file_path);
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    match parse_dicom_file(&input) {
        Ok((_, dicom_file)) => println!("{:?}", dicom_file),
        Err(err) => println!("Error parsing DICOM file: {:?}", err),
    }
}