use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0, many_m_n},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TiffHeader {
    byte_order: u16,
    version: u16,
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

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = le_u16(input)?;
    let (input, version) = le_u16(input)?;
    let (input, ifd_offset) = le_u32(input)?;

    Ok((input, TiffHeader {
        byte_order,
        version,
        ifd_offset,
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

fn parse_image_file_directory(input: &[u8]) -> IResult<&[u8], ImageFileDirectory> {
    let (input, num_entries) = le_u16(input)?;
    let (input, entries) = count(parse_ifd_entry, num_entries as usize)(input)?;
    let (input, next_ifd_offset) = opt(le_u32)(input)?;

    Ok((input, ImageFileDirectory {
        entries,
        next_ifd_offset,
    }))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], (TiffHeader, Vec<ImageFileDirectory>)> {
    let (input, header) = parse_tiff_header(input)?;
    let mut remaining_input = &input[header.ifd_offset as usize..];
    let mut directories = Vec::new();

    loop {
        let (next_input, directory) = parse_image_file_directory(remaining_input)?;
        directories.push(directory);

        if let Some(next_offset) = directories.last().unwrap().next_ifd_offset {
            remaining_input = &input[next_offset as usize..];
        } else {
            break;
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
            for (i, dir) in directories.iter().enumerate() {
                println!("Directory {}: {:?}", i, dir);
            }
        }
        Err(e) => {
            eprintln!("Error parsing TIFF: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}