use nom::{
    bytes::complete::take,
    combinator::opt,
    multi::count,
    number::complete::{
        le_u16, le_u32, be_u16, be_u32,
    },
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
struct IFDEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_or_offset: u32,
}

#[derive(Debug)]
struct ImageFileDirectory {
    entries: Vec<IFDEntry>,
    next_ifd_offset: Option<u32>,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], ByteOrder> {
    let (input, order_bytes) = take(2usize)(input)?;
    match order_bytes {
        b"II" => Ok((input, ByteOrder::LittleEndian)),
        b"MM" => Ok((input, ByteOrder::BigEndian)),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
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

fn parse_ifd_entry(input: &[u8], byte_order: ByteOrder) -> IResult<&[u8], IFDEntry> {
    let (input, tag) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?,
    };
    let (input, field_type) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?,
    };
    let (input, count) = match byte_order {
        ByteOrder::LittleEndian => le_u32(input)?,
        ByteOrder::BigEndian => be_u32(input)?,
    };
    let (input, value_or_offset) = match byte_order {
        ByteOrder::LittleEndian => le_u32(input)?,
        ByteOrder::BigEndian => be_u32(input)?,
    };

    Ok((input, IFDEntry {
        tag,
        field_type,
        count,
        value_or_offset,
    }))
}

fn parse_image_file_directory(input: &[u8], byte_order: ByteOrder) -> IResult<&[u8], ImageFileDirectory> {
    let (input, num_entries) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?,
    };

    let (input, entries) = count(
        |i| parse_ifd_entry(i, byte_order.clone()),
        num_entries as usize
    )(input)?;

    let (input, next_ifd_offset) = match byte_order {
        ByteOrder::LittleEndian => opt(le_u32)(input)?,
        ByteOrder::BigEndian => opt(be_u32)(input)?,
    };

    Ok((input, ImageFileDirectory {
        entries,
        next_ifd_offset,
    }))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], (TiffHeader, ImageFileDirectory)> {
    let (input, header) = parse_tiff_header(input)?;
    let (input, ifd) = parse_image_file_directory(&input[header.ifd_offset as usize..], header.byte_order.clone())?;
    Ok((input, (header, ifd)))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff(&buffer) {
        Ok((_, (header, ifd))) => {
            println!("TIFF Header: {:?}", header);
            println!("Image File Directory: {:?}", ifd);
        },
        Err(e) => {
            eprintln!("Parsing error: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}