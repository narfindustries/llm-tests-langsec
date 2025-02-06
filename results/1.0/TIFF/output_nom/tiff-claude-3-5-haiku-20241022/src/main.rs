use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    IResult,
    bytes::complete::{take},
    number::complete::{
        le_u16, le_u32, be_u16, be_u32,
    },
    multi::{count},
};

#[derive(Debug, Clone)]
enum ByteOrder {
    LittleEndian,
    BigEndian
}

#[derive(Debug)]
struct TiffHeader<'a> {
    byte_order: ByteOrder,
    version: u16,
    ifd_offset: u32,
    input: &'a [u8]
}

#[derive(Debug)]
struct IFDEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_or_offset: u32
}

#[derive(Debug)]
struct ImageFileDirectory {
    entries: Vec<IFDEntry>
}

fn parse_byte_order<'a>(input: &'a [u8]) -> IResult<&'a [u8], ByteOrder> {
    let (input, order) = take(2usize)(input)?;
    match order {
        b"II" => Ok((input, ByteOrder::LittleEndian)),
        b"MM" => Ok((input, ByteOrder::BigEndian)),
        _ => Err(nom::Err::Error(nom::error::make_error(input, nom::error::ErrorKind::Tag)))
    }
}

fn parse_tiff_header<'a>(input: &'a [u8]) -> IResult<&'a [u8], TiffHeader<'a>> {
    let (input, byte_order) = parse_byte_order(input)?;
    let (input, version) = match &byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?
    };
    let (input, ifd_offset) = match &byte_order {
        ByteOrder::LittleEndian => le_u32(input)?,
        ByteOrder::BigEndian => be_u32(input)?
    };
    Ok((input, TiffHeader { byte_order, version, ifd_offset, input }))
}

fn parse_ifd_entry<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], IFDEntry> {
    let (input, tag) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?
    };
    let (input, field_type) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?
    };
    let (input, count) = match byte_order {
        ByteOrder::LittleEndian => le_u32(input)?,
        ByteOrder::BigEndian => be_u32(input)?
    };
    let (input, value_or_offset) = match byte_order {
        ByteOrder::LittleEndian => le_u32(input)?,
        ByteOrder::BigEndian => be_u32(input)?
    };
    Ok((input, IFDEntry { tag, field_type, count, value_or_offset }))
}

fn parse_ifd<'a>(input: &'a [u8], byte_order: &ByteOrder) -> IResult<&'a [u8], ImageFileDirectory> {
    let (input, num_entries) = match byte_order {
        ByteOrder::LittleEndian => le_u16(input)?,
        ByteOrder::BigEndian => be_u16(input)?
    };
    let (input, entries) = count(|i| parse_ifd_entry(i, byte_order), num_entries as usize)(input)?;
    Ok((input, ImageFileDirectory { entries }))
}

fn parse_tiff<'a>(input: &'a [u8]) -> IResult<&'a [u8], (TiffHeader<'a>, Vec<ImageFileDirectory>)> {
    let (input, header) = parse_tiff_header(input)?;
    let mut current_input = &input[header.ifd_offset as usize..];
    let mut directories = Vec::new();

    loop {
        let (remaining, directory) = parse_ifd(current_input, &header.byte_order)?;
        directories.push(directory);

        if remaining.is_empty() {
            break;
        }
        current_input = remaining;
    }

    Ok((input, (header, directories)))
}

fn main() -> std::io::Result<()> {
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
            println!("Directories: {:?}", directories);
        },
        Err(e) => eprintln!("Parsing error: {:?}", e)
    }

    Ok(())
}