use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    error::{context, ErrorKind, ParseError},
    number::complete::{be_u16, be_u32},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug, PartialEq)]
enum TiffDataType {
    Byte,
    Ascii,
    Short,
    Long,
    Rational,
    SByte,
    Undefined,
    SShort,
    SLong,
    SRational,
    Float,
    Double,
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

#[derive(Debug, PartialEq)]
struct TiffField {
    tag: u16,
    data_type: TiffDataType,
    count: u32,
    value: Vec<u8>,
}


fn tiff_header(input: &[u8]) -> IResult<&[u8], (u16, u32)> {
    context(
        "TIFF header",
        tuple((be_u16, preceded(tag(b"II"), be_u32))),
    )(input)
}

fn tiff_ifd_entry(input: &[u8]) -> IResult<&[u8], TiffField> {
    let (input, (tag, data_type, count, value_offset)) = tuple((
        be_u16,
        map(be_u16, |dt| TiffDataType::from_u16(dt).unwrap()),
        be_u32,
        be_u32,
    ))(input)?;

    Ok((input, TiffField { tag, data_type, count, value: vec![] }))
}

fn tiff_ifd(input: &[u8]) -> IResult<&[u8], Vec<TiffField>> {
    let (input, num_entries) = be_u16(input)?;
    let (input, entries) =  take(num_entries as usize * 12)(input)?;
    Ok((input, vec![])) //TODO: process entries
}


fn parse_tiff(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, (byte_order, offset)) = tiff_header(input)?;
    Ok((input, ())) //TODO: process IFD using offset
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: tiff_parser <filename>");
        return;
    }

    let filename = &args[1];
    let path = Path::new(filename);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            return;
        }
    };

    match parse_tiff(&buffer) {
        Ok((remaining, _)) => {
            println!("TIFF parsing successful!");
            if !remaining.is_empty() {
                println!("Remaining bytes: {:?}", remaining);
            }
        }
        Err(e) => {
            eprintln!("TIFF parsing failed: {:?}", e);
        }
    }
}
