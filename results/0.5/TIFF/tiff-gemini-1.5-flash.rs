use nom::{
    bytes::complete::{tag, take},
    number::complete::{be_u16, be_u32},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;


#[derive(Debug)]
enum TiffDataType {
    Byte,
    Ascii,
    Short,
    Long,
    Rational,
    SByte,
    SShort,
    SLong,
    SRational,
    Float,
    Double,
    Undefined,
}

impl TiffDataType {
    fn from_u16(value: u16) -> Option<Self> {
        match value {
            1 => Some(TiffDataType::Byte),
            2 => Some(TiffDataType::Ascii),
            3 => Some(TiffDataType::Short),
            4 => Some(TiffDataType::Long),
            5 => Some(TiffDataType::Rational),
            6 => Some(TiffDataType::SByte),
            7 => Some(TiffDataType::Undefined),
            8 => Some(TiffDataType::SShort),
            9 => Some(TiffDataType::SLong),
            10 => Some(TiffDataType::SRational),
            11 => Some(TiffDataType::Float),
            12 => Some(TiffDataType::Double),
            _ => None,
        }
    }
}


#[derive(Debug)]
struct TiffField {
    tag: u16,
    data_type: TiffDataType,
    count: u32,
    value: Vec<u8>,
}

fn tiff_field(input: &[u8]) -> IResult<&[u8], TiffField> {
    let (input, (tag, data_type, count)) = tuple((be_u16, be_u16, be_u32))(input)?;
    let data_type = match TiffDataType::from_u16(data_type) {
        Some(dt) => dt,
        None => return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Verify))),
    };
    let value_len = match data_type {
        TiffDataType::Byte | TiffDataType::Ascii | TiffDataType::SByte | TiffDataType::Undefined => count,
        TiffDataType::Short | TiffDataType::SShort => count * 2,
        TiffDataType::Long | TiffDataType::SLong | TiffDataType::Float => count * 4,
        TiffDataType::Rational | TiffDataType::SRational | TiffDataType::Double => count * 8,
    };

    let (input, value) = take(value_len as usize)(input)?;

    Ok((input, TiffField { tag, data_type, count, value: value.to_vec() }))
}

fn tiff_header(input: &[u8]) -> IResult<&[u8], (u32, u32)> {
    let (input, _) = tag(b"II")(input)?; // or "MM"
    let (input, version) = be_u16(input)?;
    if version != 42 {
        return Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Verify)));
    }
    let (input, offset) = be_u32(input)?;
    Ok((input, (version as u32, offset)))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <tiff_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match tiff_header(&buffer) {
        Ok((remaining, (version, offset))) => {
            println!("TIFF Version: {}, Offset: {}", version, offset);
            let mut input = &remaining[offset as usize..];
            loop {
                match tiff_field(input) {
                    Ok((next_input, field)) => {
                        println!("Field: {:?}", field);
                        input = next_input;
                    }
                    Err(nom::Err::Incomplete(_)) => break,
                    Err(e) => {
                        println!("Error parsing TIFF field: {:?}", e);
                        break;
                    }
                }
            }
        }
        Err(e) => println!("Error parsing TIFF header: {:?}", e),
    }
}
