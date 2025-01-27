use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_data, many0},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug, PartialEq)]
enum VR {
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
    UN,
    UR,
    US,
    UT,
}

#[derive(Debug)]
struct Element {
    tag: (u16, u16),
    vr: VR,
    length: u32,
    value: Vec<u8>,
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], VR> {
    let (input, vr) = take(2u8)(input)?;
    match vr {
        b"AE" => Ok((input, VR::AE)),
        b"AS" => Ok((input, VR::AS)),
        b"AT" => Ok((input, VR::AT)),
        b"CS" => Ok((input, VR::CS)),
        b"DA" => Ok((input, VR::DA)),
        b"DS" => Ok((input, VR::DS)),
        b"DT" => Ok((input, VR::DT)),
        b"FL" => Ok((input, VR::FL)),
        b"FD" => Ok((input, VR::FD)),
        b"IS" => Ok((input, VR::IS)),
        b"LO" => Ok((input, VR::LO)),
        b"LT" => Ok((input, VR::LT)),
        b"OB" => Ok((input, VR::OB)),
        b"OD" => Ok((input, VR::OD)),
        b"OF" => Ok((input, VR::OF)),
        b"OL" => Ok((input, VR::OL)),
        b"OW" => Ok((input, VR::OW)),
        b"PN" => Ok((input, VR::PN)),
        b"SH" => Ok((input, VR::SH)),
        b"SL" => Ok((input, VR::SL)),
        b"SQ" => Ok((input, VR::SQ)),
        b"SS" => Ok((input, VR::SS)),
        b"ST" => Ok((input, VR::ST)),
        b"TM" => Ok((input, VR::TM)),
        b"UI" => Ok((input, VR::UI)),
        b"UL" => Ok((input, VR::UL)),
        b"UN" => Ok((input, VR::UN)),
        b"UR" => Ok((input, VR::UR)),
        b"US" => Ok((input, VR::US)),
        b"UT" => Ok((input, VR::UT)),
        _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
    }
}

fn parse_tag(input: &[u8]) -> IResult<&[u8], (u16, u16)> {
    let (input, group) = be_u16(input)?;
    let (input, element) = be_u16(input)?;
    Ok((input, (group, element)))
}

fn parse_length(input: &[u8]) -> IResult<&[u8], u32> {
    let (input, length) = be_u32(input)?;
    Ok((input, length))
}

fn parse_element(input: &[u8]) -> IResult<&[u8], Element> {
    let (input, tag) = parse_tag(input)?;
    let (input, vr) = parse_vr(input)?;
    let (input, length) = parse_length(input)?;
    let (input, value) = take(length as usize)(input)?;
    Ok((input, Element { tag, vr, length, value }))
}

fn parse_file(input: &[u8]) -> IResult<&[u8], Vec<Element>> {
    let (input, _) = tag("\x08\x00\x00\x00\x08\x00\x00\x00")(input)?;
    many0(parse_element)(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let file = File::open(file_path).unwrap();
    let mut reader = BufReader::new(file);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).unwrap();
    let (_, elements) = parse_file(&buffer).unwrap();
    for element in elements {
        println!("{:?}", element);
    }
}