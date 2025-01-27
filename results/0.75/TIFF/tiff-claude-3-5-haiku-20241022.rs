use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    multi::{count, many0, many_m_n},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{pair, tuple},
    IResult,
    error::ErrorKind,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TiffHeader {
    byte_order: u16,
    magic_number: u16,
    first_ifd_offset: u32,
}

#[derive(Debug)]
struct IFDEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_or_offset: u32,
}

#[derive(Debug)]
struct IFD {
    entry_count: u16,
    entries: Vec<IFDEntry>,
    next_ifd_offset: u32,
}

#[derive(Debug)]
struct TiffFile {
    header: TiffHeader,
    ifds: Vec<IFD>,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = le_u16(input)?;
    let (input, magic_number) = le_u16(input)?;
    let (input, first_ifd_offset) = le_u32(input)?;

    Ok((input, TiffHeader {
        byte_order,
        magic_number,
        first_ifd_offset,
    }))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IFDEntry> {
    let (input, tag) = le_u16(input)?;
    let (input, field_type) = le_u16(input)?;
    let (input, count) = le_u32(input)?;
    let (input, value_or_offset) = le_u32(input)?;

    Ok((input, IFDEntry {
        tag,
        field_type,
        count,
        value_or_offset,
    }))
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], IFD> {
    let (input, entry_count) = le_u16(input)?;
    let (input, entries) = count(parse_ifd_entry, entry_count as usize)(input)?;
    let (input, next_ifd_offset) = le_u32(input)?;

    Ok((input, IFD {
        entry_count,
        entries,
        next_ifd_offset,
    }))
}

fn parse_tiff_file(input: &[u8]) -> IResult<&[u8], TiffFile> {
    let (input, header) = parse_tiff_header(input)?;
    let mut ifds = Vec::new();
    let mut current_input = input;

    while !current_input.is_empty() {
        let (remaining_input, ifd) = parse_ifd(current_input)?;
        ifds.push(ifd);
        current_input = remaining_input;
    }

    Ok((current_input, TiffFile {
        header,
        ifds,
    }))
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

    match parse_tiff_file(&buffer) {
        Ok((_, tiff_file)) => {
            println!("TIFF File parsed successfully: {:?}", tiff_file);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing TIFF file: {:?}", e);
            std::process::exit(1);
        }
    }
}