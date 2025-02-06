use nom::{
    bytes::complete::take,
    combinator::opt,
    multi::count,
    number::complete::{le_u16, le_u32, be_u16, be_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, Clone)]
enum ByteOrder {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
struct TiffHeader {
    byte_order: ByteOrder,
    magic_number: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct Rational {
    numerator: u32,
    denominator: u32,
}

#[derive(Debug)]
enum TagValue {
    Byte(u8),
    Ascii(String),
    Short(u16),
    Long(u32),
    Rational(Rational),
    SByte(i8),
    Undefined(Vec<u8>),
    SShort(i16),
    SLong(i32),
    SRational(Rational),
    Float(f32),
    Double(f64),
}

#[derive(Debug, Clone)]
struct IFDEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_or_offset: u32,
}

#[derive(Debug, Clone)]
struct ImageFileDirectory {
    entries: Vec<IFDEntry>,
    next_ifd_offset: Option<u32>,
}

fn parse_byte_order<'a>(input: &'a [u8]) -> IResult<&'a [u8], ByteOrder> {
    let (input, order_bytes) = take(2usize)(input)?;
    match order_bytes {
        b"II" => Ok((input, ByteOrder::LittleEndian)),
        b"MM" => Ok((input, ByteOrder::BigEndian)),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_tiff_header<'a>(input: &'a [u8]) -> IResult<&'a [u8], TiffHeader> {
    let (input, byte_order) = parse_byte_order(input)?;
    let (input, magic_number) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?,
    };
    let (input, ifd_offset) = match byte_order {
        ByteOrder::LittleEndian => le_u32(input)?,
        ByteOrder::BigEndian => be_u32(input)?,
    };

    Ok((input, TiffHeader {
        byte_order,
        magic_number,
        ifd_offset,
    }))
}

fn parse_ifd_entry<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], IFDEntry> {
    let parse_u16 = match byte_order {
        ByteOrder::LittleEndian => le_u16,
        ByteOrder::BigEndian => be_u16,
    };
    let parse_u32 = match byte_order {
        ByteOrder::LittleEndian => le_u32,
        ByteOrder::BigEndian => be_u32,
    };

    let (input, tag) = parse_u16(input)?;
    let (input, field_type) = parse_u16(input)?;
    let (input, count) = parse_u32(input)?;
    let (input, value_or_offset) = parse_u32(input)?;

    Ok((input, IFDEntry {
        tag,
        field_type,
        count,
        value_or_offset,
    }))
}

fn parse_ifd<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], ImageFileDirectory> {
    let parse_u16 = match byte_order {
        ByteOrder::LittleEndian => le_u16,
        ByteOrder::BigEndian => be_u16,
    };
    let parse_u32 = match byte_order {
        ByteOrder::LittleEndian => le_u32,
        ByteOrder::BigEndian => be_u32,
    };

    let (input, entry_count) = parse_u16(input)?;
    let (input, entries) = count(
        |i| parse_ifd_entry(i, byte_order),
        entry_count as usize
    )(input)?;

    let (input, next_ifd_offset) = opt(parse_u32)(input)?;

    Ok((input, ImageFileDirectory {
        entries,
        next_ifd_offset,
    }))
}

fn parse_tiff<'a>(input: &'a [u8]) -> IResult<&'a [u8], (TiffHeader, Vec<ImageFileDirectory>)> {
    let (input, header) = parse_tiff_header(input)?;
    
    let mut current_input = &input[header.ifd_offset as usize..];
    let mut directories = Vec::new();

    loop {
        let (remaining, directory) = parse_ifd(current_input, &header.byte_order)?;
        directories.push(directory);

        match directory.next_ifd_offset {
            Some(offset) => current_input = &input[offset as usize..],
            None => break,
        }
    }

    Ok((input, (header, directories)))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff(&buffer) {
        Ok((_, (header, directories))) => {
            println!("TIFF Header: {:?}", header);
            for (idx, dir) in directories.iter().enumerate() {
                println!("Directory {}: {:?}", idx, dir);
            }
        },
        Err(e) => eprintln!("Parsing error: {:?}", e),
    }

    Ok(())
}