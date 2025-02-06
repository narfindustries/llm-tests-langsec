use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct TiffHeader {
    byte_order: u16,
    magic: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct IfdEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct TiffFile {
    header: TiffHeader,
    ifd_entries: Vec<IfdEntry>,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, (byte_order, magic, ifd_offset)) = tuple((le_u16, le_u16, le_u32))(input)?;
    Ok((
        input,
        TiffHeader {
            byte_order,
            magic,
            ifd_offset,
        },
    ))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) = tuple((le_u16, le_u16, le_u32, le_u32))(input)?;
    Ok((
        input,
        IfdEntry {
            tag,
            field_type,
            count,
            value_offset,
        },
    ))
}

fn parse_ifd_entries(input: &[u8], count: u16) -> IResult<&[u8], Vec<IfdEntry>> {
    let mut entries = Vec::new();
    let mut remaining_input = input;
    for _ in 0..count {
        let (input, entry) = parse_ifd_entry(remaining_input)?;
        entries.push(entry);
        remaining_input = input;
    }
    Ok((remaining_input, entries))
}

fn parse_tiff_file(input: &[u8]) -> IResult<&[u8], TiffFile> {
    let (input, header) = parse_tiff_header(input)?;
    let ifd_offset = header.ifd_offset as usize;
    let (input, ifd_count) = le_u16(&input[ifd_offset..])?;
    let (input, ifd_entries) = parse_ifd_entries(&input[ifd_offset + 2..], ifd_count)?;
    Ok((
        input,
        TiffFile {
            header,
            ifd_entries,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_tiff_file(&buffer) {
        Ok((_, tiff_file)) => {
            println!("{:?}", tiff_file);
        }
        Err(e) => {
            eprintln!("Failed to parse TIFF file: {:?}", e);
        }
    }
}