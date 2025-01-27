use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{many0, many1},
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

#[derive(Debug, PartialEq)]
struct Element {
    tag: (u16, u16),
    vr: VR,
    length: u32,
    value: Vec<u8>,
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], VR> {
    map(
        take(2u8),
        |vr: &[u8]| match vr {
            b"AE" => VR::AE,
            b"AS" => VR::AS,
            b"AT" => VR::AT,
            b"CS" => VR::CS,
            b"DA" => VR::DA,
            b"DS" => VR::DS,
            b"DT" => VR::DT,
            b"FL" => VR::FL,
            b"FD" => VR::FD,
            b"IS" => VR::IS,
            b"LO" => VR::LO,
            b"LT" => VR::LT,
            b"OB" => VR::OB,
            b"OD" => VR::OD,
            b"OF" => VR::OF,
            b"OW" => VR::OW,
            b"PN" => VR::PN,
            b"SH" => VR::SH,
            b"SL" => VR::SL,
            b"SQ" => VR::SQ,
            b"SS" => VR::SS,
            b"ST" => VR::ST,
            b"TM" => VR::TM,
            b"UI" => VR::UI,
            b"UL" => VR::UL,
            b"UN" => VR::UN,
            b"UR" => VR::UR,
            b"US" => VR::US,
            b"UT" => VR::UT,
            _ => panic!("Invalid VR"),
        },
    )(input)
}

fn parse_element(input: &[u8]) -> IResult<&[u8], Element> {
    map(
        tuple((be_u16, be_u16, parse_vr, be_u32, take(4u8))),
        |(group, element, vr, length, _)| Element {
            tag: (group, element),
            vr,
            length,
            value: vec![],
        },
    )(input)
}

fn parse_file(input: &[u8]) -> IResult<&[u8], Vec<Element>> {
    many1(parse_element)(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (_, elements) = parse_file(&input).unwrap();
    for element in elements {
        println!("{:?}", element);
    }
}