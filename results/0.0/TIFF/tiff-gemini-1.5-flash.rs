use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, verify},
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


#[derive(Debug)]
struct TiffField {
    tag: u16,
    data_type: TiffDataType,
    count: u32,
    value: Vec<u8>,
}

fn tiff_header(input: &[u8]) -> IResult<&[u8], (u16, u32)> {
    tuple((be_u16, be_u32))(input)
}

fn tiff_ifd(input: &[u8]) -> IResult<&[u8], Vec<TiffField>> {
    let (input, num_entries) = be_u16(input)?;
    let mut fields = Vec::new();
    let mut remaining_input = input;
    for _ in 0..num_entries {
        let (new_input, field) = tiff_field(remaining_input)?;
        fields.push(field);
        remaining_input = new_input;
    }
    Ok((remaining_input, fields))
}

fn tiff_field(input: &[u8]) -> IResult<&[u8], TiffField> {
    let (input, (tag, data_type, count, value_offset)) = tuple((
        be_u16,
        map(be_u16, |dt| TiffDataType::from_u16(dt).unwrap()),
        be_u32,
        be_u32,
    ))(input)?;

    let value = if value_offset == 0 {
        // Value is inline
        let data_size = match data_type {
            TiffDataType::Byte | TiffDataType::SByte | TiffDataType::Ascii | TiffDataType::Undefined => count,
            TiffDataType::Short | TiffDataType::SShort => count * 2,
            TiffDataType::Long | TiffDataType::SLong => count * 4,
            TiffDataType::Rational | TiffDataType::SRational => count * 8,
            TiffDataType::Float => count * 4,
            TiffDataType::Double => count * 8,
        };
        let (input, value) = take(data_size as usize)(input)?;
        value.to_vec()
    } else {
        // Value is at offset
        // This is a simplification, needs actual file reading
        vec![]
    };

    Ok((input, TiffField { tag, data_type, count, value }))
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
        Ok((remaining, (byte_order, offset))) => {
            println!("Byte Order: {:?}", byte_order);
            println!("Offset: {:?}", offset);
            // Process IFD here using offset
        }
        Err(e) => {
            println!("Error parsing TIFF header: {:?}", e);
        }
    }
}
