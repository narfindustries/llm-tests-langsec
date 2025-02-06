use std::fs::File;
use std::io::Read;
use std::path::Path;
use nom::{
    IResult,
    bytes::complete::take,
    number::complete::{le_u16, le_u32, be_u16, be_u32},
};

#[derive(Debug)]
pub struct TiffHeader {
    byte_order: ByteOrder,
    version: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
pub enum ByteOrder {
    Intel,
    Motorola,
}

#[derive(Debug)]
pub struct IFDEntry {
    tag: u16,
    field_type: FieldType,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
pub enum FieldType {
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

impl FieldType {
    fn from_u16(value: u16) -> Option<FieldType> {
        match value {
            1 => Some(FieldType::Byte),
            2 => Some(FieldType::Ascii),
            3 => Some(FieldType::Short),
            4 => Some(FieldType::Long),
            5 => Some(FieldType::Rational),
            6 => Some(FieldType::SByte),
            7 => Some(FieldType::Undefined),
            8 => Some(FieldType::SShort),
            9 => Some(FieldType::SLong),
            10 => Some(FieldType::SRational),
            11 => Some(FieldType::Float),
            12 => Some(FieldType::Double),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub struct IFD {
    entry_count: u16,
    entries: Vec<IFDEntry>,
    next_ifd_offset: u32,
}

fn parse_byte_order<'a>(input: &'a [u8]) -> IResult<&'a [u8], ByteOrder> {
    let (input, order_bytes) = take(2usize)(input)?;
    match order_bytes {
        b"II" => Ok((input, ByteOrder::Intel)),
        b"MM" => Ok((input, ByteOrder::Motorola)),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_tiff_header<'a>(input: &'a [u8]) -> IResult<&'a [u8], TiffHeader> {
    let (input, byte_order) = parse_byte_order(input)?;
    let (input, version) = match byte_order {
        ByteOrder::Intel => le_u16(input)?,
        ByteOrder::Motorola => be_u16(input)?,
    };
    let (input, ifd_offset) = match byte_order {
        ByteOrder::Intel => le_u32(input)?,
        ByteOrder::Motorola => be_u32(input)?,
    };

    Ok((
        input,
        TiffHeader {
            byte_order,
            version,
            ifd_offset,
        },
    ))
}

fn parse_ifd_entry<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], IFDEntry> {
    let (input, tag) = match byte_order {
        ByteOrder::Intel => le_u16(input)?,
        ByteOrder::Motorola => be_u16(input)?,
    };
    let (input, type_value) = match byte_order {
        ByteOrder::Intel => le_u16(input)?,
        ByteOrder::Motorola => be_u16(input)?,
    };
    let field_type = FieldType::from_u16(type_value).unwrap();
    
    let (input, count) = match byte_order {
        ByteOrder::Intel => le_u32(input)?,
        ByteOrder::Motorola => be_u32(input)?,
    };
    let (input, value_offset) = match byte_order {
        ByteOrder::Intel => le_u32(input)?,
        ByteOrder::Motorola => be_u32(input)?,
    };

    Ok((
        input,
        IFDEntry {
            tag,
            field_type,
            count,
            value_offset,
        },
    ))
}

fn parse_ifd<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], IFD> {
    let (input, entry_count) = match byte_order {
        ByteOrder::Intel => le_u16(input)?,
        ByteOrder::Motorola => be_u16(input)?,
    };

    let mut entries = Vec::new();
    let mut remaining = input;

    for _ in 0..entry_count {
        let (new_remaining, entry) = parse_ifd_entry(remaining, byte_order)?;
        entries.push(entry);
        remaining = new_remaining;
    }

    let (remaining, next_ifd_offset) = match byte_order {
        ByteOrder::Intel => le_u32(remaining)?,
        ByteOrder::Motorola => be_u32(remaining)?,
    };

    Ok((
        remaining,
        IFD {
            entry_count,
            entries,
            next_ifd_offset,
        },
    ))
}

fn parse_tiff<'a>(input: &'a [u8]) -> IResult<&'a [u8], (TiffHeader, IFD)> {
    let (input, header) = parse_tiff_header(input)?;
    let (input, ifd) = parse_ifd(input, &header.byte_order)?;
    Ok((input, (header, ifd)))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff(&buffer) {
        Ok((_, (header, ifd))) => {
            println!("TIFF Header: {:?}", header);
            println!("IFD: {:?}", ifd);
        }
        Err(e) => {
            eprintln!("Error parsing TIFF: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}