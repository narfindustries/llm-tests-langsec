use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    number::complete::{be_u16, be_u32},
    sequence::{pair, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
    pair(be_u16, be_u32)(input)
}

fn tiff_ifd(input: &[u8]) -> IResult<&[u8], Vec<TiffField>> {
    let (input, num_entries) = be_u16(input)?;
    let mut entries = Vec::new();
    let mut remaining_input = input;
    for _ in 0..num_entries {
        let (new_input, entry) = tiff_entry(remaining_input)?;
        entries.push(entry);
        remaining_input = new_input;
    }
    let (input, next_ifd_offset) = be_u32(remaining_input)?;
    Ok((input, entries))
}

fn tiff_entry(input: &[u8]) -> IResult<&[u8], TiffField> {
    let (input, (tag, data_type, count, value_offset)) = tuple((be_u16, be_u16, be_u32, be_u32))(input)?;
    let data_type = TiffDataType::from_u16(data_type).unwrap(); //Handle errors appropriately in production code.
    let value = if value_offset == 0 {
        match data_type {
            TiffDataType::Byte | TiffDataType::Ascii | TiffDataType::Short | TiffDataType::Long | TiffDataType::SByte | TiffDataType::Undefined | TiffDataType::SShort | TiffDataType::SLong => {
                let value_size = match data_type {
                    TiffDataType::Byte | TiffDataType::SByte | TiffDataType::Undefined => 1,
                    TiffDataType::Ascii | TiffDataType::Short | TiffDataType::SShort => 2,
                    TiffDataType::Long | TiffDataType::SLong => 4,
                    _ => 0, // Handle other types appropriately
                };
                take(value_size * count as usize)(input)?.1.to_vec()
            }
            _ => vec![], // Handle other types appropriately
        }
    } else {
        //Handle reading from offset
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

    let result = tiff_header(&buffer);
    match result {
        Ok((remaining, (byte_order, first_ifd_offset))) => {
            println!("Byte Order: {:?}", byte_order);
            println!("First IFD Offset: {:?}", first_ifd_offset);
            //Further parsing of IFDs would go here.
        }
        Err(e) => {
            println!("Error parsing header: {:?}", e);
        }
    }
}
