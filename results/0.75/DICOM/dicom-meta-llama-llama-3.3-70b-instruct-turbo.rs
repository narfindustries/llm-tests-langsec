use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_opt},
    multi::many0,
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read},
    path::Path,
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
    UX,
}

#[derive(Debug, PartialEq)]
struct DataElement {
    tag: (u16, u16),
    vr: VR,
    value: Vec<u8>,
}

impl DataElement {
    fn new(tag: (u16, u16), vr: VR, value: Vec<u8>) -> Self {
        DataElement { tag, vr, value }
    }
}

fn parse_vr(input: &[u8]) -> IResult<&[u8], VR> {
    map_opt(
        take(2usize),
        |vr: &[u8]| match std::str::from_utf8(vr) {
            Ok("AE") => Some(VR::AE),
            Ok("AS") => Some(VR::AS),
            Ok("AT") => Some(VR::AT),
            Ok("CS") => Some(VR::CS),
            Ok("DA") => Some(VR::DA),
            Ok("DS") => Some(VR::DS),
            Ok("DT") => Some(VR::DT),
            Ok("FL") => Some(VR::FL),
            Ok("FD") => Some(VR::FD),
            Ok("IS") => Some(VR::IS),
            Ok("LO") => Some(VR::LO),
            Ok("LT") => Some(VR::LT),
            Ok("OB") => Some(VR::OB),
            Ok("OD") => Some(VR::OD),
            Ok("OF") => Some(VR::OF),
            Ok("OL") => Some(VR::OL),
            Ok("OW") => Some(VR::OW),
            Ok("PN") => Some(VR::PN),
            Ok("SH") => Some(VR::SH),
            Ok("SL") => Some(VR::SL),
            Ok("SQ") => Some(VR::SQ),
            Ok("SS") => Some(VR::SS),
            Ok("ST") => Some(VR::ST),
            Ok("TM") => Some(VR::TM),
            Ok("UI") => Some(VR::UI),
            Ok("UL") => Some(VR::UL),
            Ok("UN") => Some(VR::UN),
            Ok("UR") => Some(VR::UR),
            Ok("US") => Some(VR::US),
            Ok("UX") => Some(VR::UX),
            _ => None,
        },
    )(input)
}

fn parse_data_element(input: &[u8]) -> IResult<&[u8], DataElement> {
    let (input, tag) = tuple((be_u16, be_u16))(input)?;
    let (input, vr) = parse_vr(input)?;
    let (_input, _value_length) = be_u32(input)?;
    let (input, value) = take(4usize)(input)?;
    Ok((input, DataElement::new(tag, vr, value.to_vec())))
}

fn parse_dicom(input: &[u8]) -> IResult<&[u8], Vec<DataElement>> {
    let (input, _) = tag("\x44\x49\x43\x4d")(input)?;
    many0(parse_data_element)(input)
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <dicom_file>", args[0]);
    }
    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    let result = parse_dicom(&buffer);
    match result {
        Ok((_, data_elements)) => {
            for data_element in data_elements {
                println!("{:?}", data_element);
            }
        }
        Err(err) => {
            println!("Error parsing DICOM file: {:?}", err);
        }
    }
    Ok(())
}