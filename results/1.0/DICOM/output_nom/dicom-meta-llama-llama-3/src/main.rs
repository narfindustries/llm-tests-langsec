extern crate nom;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{be_u16, be_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::{
    collections::HashMap,
    fs::File,
    io::{BufReader, Read},
    path::Path,
};
use std::env;

#[derive(Debug, PartialEq, Eq)]
enum Vr {
    AE,
    AS,
    AT,
    CS,
    DA,
    DS,
    DT,
    FL,
    FD,
    IS,
    LO,
    LT,
    OB,
    OD,
    OF,
    OL,
    OW,
    PN,
    SH,
    SL,
    SQ,
    SS,
    ST,
    TM,
    UI,
    UL,
    UR,
    US,
    UT,
}

#[derive(Debug, PartialEq, Eq)]
struct DataElement {
    tag: (u16, u16),
    vr: Vr,
    length: u32,
    value: Vec<u8>,
}

#[derive(Debug, PartialEq, Eq)]
struct DicomFile {
    header: Vec<DataElement>,
    meta: Vec<DataElement>,
    data: Vec<DataElement>,
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], Vr> {
    preceded(
        take(2u8),
        nom::branch::alt((
            tag("AE").map(|_| Vr::AE),
            tag("AS").map(|_| Vr::AS),
            tag("AT").map(|_| Vr::AT),
            tag("CS").map(|_| Vr::CS),
            tag("DA").map(|_| Vr::DA),
            tag("FL").map(|_| Vr::FL),
            tag("FD").map(|_| Vr::FD),
            tag("IS").map(|_| Vr::IS),
            tag("LO").map(|_| Vr::LO),
            tag("LT").map(|_| Vr::LT),
            tag("OB").map(|_| Vr::OB),
            tag("OD").map(|_| Vr::OD),
            tag("OF").map(|_| Vr::OF),
            tag("OL").map(|_| Vr::OL),
            tag("OW").map(|_| Vr::OW),
            tag("PN").map(|_| Vr::PN),
            tag("SH").map(|_| Vr::SH),
            tag("SL").map(|_| Vr::SL),
            tag("SQ").map(|_| Vr::SQ),
            tag("SS").map(|_| Vr::SS),
            tag("ST").map(|_| Vr::ST),
            tag("TM").map(|_| Vr::TM),
            tag("UI").map(|_| Vr::UI),
            tag("UL").map(|_| Vr::UL),
            tag("UR").map(|_| Vr::UR),
            tag("US").map(|_| Vr::US),
            tag("UT").map(|_| Vr::UT),
        )),
    )(input)
}

fn parse_tag(input: &[u8]) -> IResult<&[u8], (u16, u16)> {
    tuple((be_u16, be_u16))(input)
}

fn parse_length(input: &[u8]) -> IResult<&[u8], u32> {
    verify(be_u32, |x: &u32| *x <= 2usize.pow(32) as u32 - 1)(input)
}

fn parse_value(input: &[u8], length: u32) -> IResult<&[u8], Vec<u8>> {
    let (input, value) = take(length)(input)?;
    Ok((input, value.to_vec()))
}

fn parse_data_element(input: &[u8]) -> IResult<&[u8], DataElement> {
    map(
        tuple((parse_tag, parse_vr, parse_length)),
        |(tag, vr, length)| {
            let (input, value) = parse_value(input, length).unwrap();
            DataElement { tag, vr, length, value }
        },
    )(input)
}

fn parse_dicom_file(input: &[u8]) -> IResult<&[u8], DicomFile> {
    map(
        tuple((
            many0(parse_data_element),
            many0(parse_data_element),
            many0(parse_data_element),
        )),
        |(header, meta, data)| DicomFile { header, meta, data },
    )(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <dicom_file>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).unwrap();
    let result = parse_dicom_file(&buffer);
    match result {
        Ok((_, dicom_file)) => println!("{:?}", dicom_file),
        Err(err) => eprintln!("Error parsing DICOM file: {:?}", err),
    }
}