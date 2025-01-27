use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::{tuple, pair},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct TiffHeader {
    byte_order: u16,
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

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    map(
        tuple((le_u16, le_u16, le_u32)),
        |(byte_order, magic_number, ifd_offset)| TiffHeader {
            byte_order,
            magic_number,
            ifd_offset,
        },
    )(input)
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IFDEntry> {
    map(
        tuple((le_u16, le_u16, le_u32, le_u32)),
        |(tag, field_type, count, value_or_offset)| IFDEntry {
            tag,
            field_type,
            count,
            value_or_offset,
        },
    )(input)
}

fn parse_image_file_directory(input: &[u8]) -> IResult<&[u8], ImageFileDirectory> {
    let (input, entry_count) = le_u16(input)?;
    let (input, entries) = count(parse_ifd_entry, entry_count as usize)(input)?;
    let (input, next_ifd_offset) = opt(le_u32)(input)?;

    Ok((
        input,
        ImageFileDirectory {
            entries,
            next_ifd_offset,
        },
    ))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], (TiffHeader, Vec<ImageFileDirectory>)> {
    let (input, header) = parse_tiff_header(input)?;
    let mut current_input = &input[header.ifd_offset as usize..];
    let mut ifds = Vec::new();

    loop {
        let (remaining, ifd) = parse_image_file_directory(current_input)?;
        ifds.push(ifd);

        if let Some(next_offset) = ifd.next_ifd_offset {
            current_input = &input[next_offset as usize..];
        } else {
            break;
        }
    }

    Ok((input, (header, ifds)))
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
        Ok((_, (header, ifds))) => {
            println!("TIFF Header: {:?}", header);
            for (i, ifd) in ifds.iter().enumerate() {
                println!("IFD {}: {:?}", i, ifd);
            }
        }
        Err(e) => {
            eprintln!("Error parsing TIFF: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}