use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{many0, many1},
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, BufReader},
};

#[derive(Debug)]
enum TIFFByteOrder {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
enum TIFFEntryType {
    Byte,
    ASCII,
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

#[derive(Debug)]
struct TIFFEntry {
    tag: u16,
    type_: TIFFEntryType,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct TIFFIFD {
    entries: Vec<TIFFEntry>,
    next_ifd_offset: u32,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], TIFFByteOrder> {
    alt((tag("II\0\0"), tag("MM\0\0")))(input).map(|(input, byte_order)| {
        let byte_order = match byte_order {
            "II\0\0" => TIFFByteOrder::LittleEndian,
            "MM\0\0" => TIFFByteOrder::BigEndian,
            _ => unreachable!(),
        };
        (input, byte_order)
    })
}

fn parse_tiff_entry_type(input: &[u8]) -> IResult<&[u8], TIFFEntryType> {
    map(be_u16, |type_| match type_ {
        1 => TIFFEntryType::Byte,
        2 => TIFFEntryType::ASCII,
        3 => TIFFEntryType::Short,
        4 => TIFFEntryType::Long,
        5 => TIFFEntryType::Rational,
        6 => TIFFEntryType::SByte,
        7 => TIFFEntryType::Undefined,
        8 => TIFFEntryType::SShort,
        9 => TIFFEntryType::SLong,
        10 => TIFFEntryType::SRational,
        11 => TIFFEntryType::Float,
        12 => TIFFEntryType::Double,
        _ => panic!("unknown TIFF entry type"),
    })(input)
}

fn parse_tiff_entry(input: &[u8], byte_order: TIFFByteOrder) -> IResult<&[u8], TIFFEntry> {
    let (input, tag) = be_u16(input)?;
    let (input, type_) = parse_tiff_entry_type(input)?;
    let (input, count) = match byte_order {
        TIFFByteOrder::LittleEndian => le_u32(input)?,
        TIFFByteOrder::BigEndian => be_u32(input)?,
    };
    let (input, value_offset) = match byte_order {
        TIFFByteOrder::LittleEndian => le_u32(input)?,
        TIFFByteOrder::BigEndian => be_u32(input)?,
    };
    Ok((input, TIFFEntry { tag, type_, count, value_offset }))
}

fn parse_tiff_ifd(input: &[u8], byte_order: TIFFByteOrder) -> IResult<&[u8], TIFFIFD> {
    let (input, entries_count) = match byte_order {
        TIFFByteOrder::LittleEndian => le_u16(input)?,
        TIFFByteOrder::BigEndian => be_u16(input)?,
    };
    let (input, entries) = many1(|input| parse_tiff_entry(input, byte_order))(input)?;
    let (input, next_ifd_offset) = match byte_order {
        TIFFByteOrder::LittleEndian => le_u32(input)?,
        TIFFByteOrder::BigEndian => be_u32(input)?,
    };
    Ok((input, TIFFIFD { entries, next_ifd_offset }))
}

fn parse_tiff_file(input: &[u8]) -> IResult<&[u8], TIFFIFD> {
    let (input, byte_order) = parse_byte_order(input)?;
    let (input, _) = be_u16(input)?; // magic number
    let (input, _) = be_u32(input)?; // offset to first IFD
    let (input, ifd) = parse_tiff_ifd(input, byte_order)?;
    Ok((input, ifd))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let file = File::open(file_path)?;
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input)?;
    let (_, ifd) = parse_tiff_file(&input).unwrap();
    println!("{:?}", ifd);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_tiff_file() {
        let input = include_bytes!("test.tiff");
        let (_, ifd) = parse_tiff_file(input).unwrap();
        println!("{:?}", ifd);
    }
}