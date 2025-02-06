use nom::{
    bytes::complete::{take},
    combinator::{map, map_res},
    multi::{many0, many1},
    number::complete::{be_u16},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};
use std::path::Path;

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
    UV,
}

#[derive(Debug, PartialEq)]
struct DataElement {
    tag: (u16, u16),
    vr: VR,
    length: u32,
    value: Vec<u8>,
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], VR> {
    map_res(
        take(2u8),
        |vr: &[u8]| match vr {
            b"AE" => Ok(VR::AE),
            b"AS" => Ok(VR::AS),
            b"AT" => Ok(VR::AT),
            b"CS" => Ok(VR::CS),
            b"DA" => Ok(VR::DA),
            b"DS" => Ok(VR::DS),
            b"DT" => Ok(VR::DT),
            b"FL" => Ok(VR::FL),
            b"FD" => Ok(VR::FD),
            b"IS" => Ok(VR::IS),
            b"LO" => Ok(VR::LO),
            b"LT" => Ok(VR::LT),
            b"OB" => Ok(VR::OB),
            b"OD" => Ok(VR::OD),
            b"OF" => Ok(VR::OF),
            b"OL" => Ok(VR::OL),
            b"OW" => Ok(VR::OW),
            b"PN" => Ok(VR::PN),
            b"SH" => Ok(VR::SH),
            b"SL" => Ok(VR::SL),
            b"SQ" => Ok(VR::SQ),
            b"SS" => Ok(VR::SS),
            b"ST" => Ok(VR::ST),
            b"TM" => Ok(VR::TM),
            b"UI" => Ok(VR::UI),
            b"UL" => Ok(VR::UL),
            b"UN" => Ok(VR::UN),
            b"UR" => Ok(VR::UR),
            b"US" => Ok(VR::US),
            b"UV" => Ok(VR::UV),
            _ => Err(nom::Err::Error(nom::error::ErrorKind::AlphaNumeric)),
        },
    )(input)
}

fn parse_tag(input: &[u8]) -> IResult<&[u8], (u16, u16)> {
    map(
        many1(be_u16),
        |tags: Vec<u16>| match tags.as_slice() {
            [group, element] => (*group, *element),
            _ => panic!("Invalid tag"),
        },
    )(input)
}

fn parse_length(input: &[u8]) -> IResult<&[u8], u32> {
    map_res(take(4u8), |length: &[u8]| {
        let mut length_array = [0u8; 4];
        length_array.copy_from_slice(length);
        Ok(u32::from_be_bytes(length_array))
    })(input)
}

fn parse_value(input: &[u8], length: u32) -> IResult<&[u8], Vec<u8>> {
    map(take(length as usize), |value: &[u8]| value.to_vec())(input)
}

fn parse_data_element(input: &[u8]) -> IResult<&[u8], DataElement> {
    map(
        tuple((parse_tag, parse_vr, parse_length)),
        |(tag, vr, length): ((u16, u16), VR, u32)| {
            let (_, value) = parse_value(input, length).unwrap();
            DataElement {
                tag,
                vr,
                length,
                value,
            }
        },
    )(input)
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], Vec<DataElement>> {
    many0(parse_data_element)(input)
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <file>", args[0]);
    }
    let file_path = Path::new(&args[1]);
    let file = File::open(file_path).expect("Failed to open file");
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).expect("Failed to read file");
    let (_, data_elements) = parse_dicom(&input).expect("Failed to parse DICOM");
    for data_element in data_elements {
        println!("{:?}", data_element);
    }
}