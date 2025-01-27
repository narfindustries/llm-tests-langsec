use nom::{
    bytes::complete::{tag, take, take_while},
    combinator::{map, opt},
    multi::{count, many0, many_m_n},
    number::complete::{le_u8, le_u16, le_u32, le_u64},
    sequence::{tuple, preceded},
    IResult,
    branch::alt,
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
struct IFD {
    entries: Vec<IFDEntry>,
    next_ifd_offset: u32,
}

#[derive(Debug)]
struct TiffFile {
    header: TiffHeader,
    ifds: Vec<IFD>,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], u16> {
    alt((
        tag(&[0x49, 0x49]),  // Little-endian
        tag(&[0x4D, 0x4D])   // Big-endian
    ))(input)
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = parse_byte_order(input)?;
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

fn parse_ifd(input: &[u8]) -> IResult<&[u8], IFD> {
    let (input, entry_count) = le_u16(input)?;
    let (input, entries) = count(parse_ifd_entry, entry_count as usize)(input)?;
    let (input, next_ifd_offset) = le_u32(input)?;

    Ok((input, IFD {
        entries,
        next_ifd_offset,
    }))
}

fn parse_tiff_file(input: &[u8]) -> IResult<&[u8], TiffFile> {
    let (input, header) = parse_tiff_header(input)?;
    let mut ifds = Vec::new();
    let mut current_input = &input[header.ifd_offset as usize..];

    loop {
        let (remaining, ifd) = parse_ifd(current_input)?;
        ifds.push(ifd);

        if ifd.next_ifd_offset == 0 {
            break;
        }
        current_input = &current_input[ifd.next_ifd_offset as usize..];
    }

    Ok((input, TiffFile {
        header,
        ifds,
    }))
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

    match parse_tiff_file(&buffer) {
        Ok((_remaining, tiff_file)) => {
            println!("TIFF File Parsed Successfully: {:?}", tiff_file);
            Ok(())
        },
        Err(e) => {
            eprintln!("Failed to parse TIFF: {:?}", e);
            Err(e.into())
        }
    }
}