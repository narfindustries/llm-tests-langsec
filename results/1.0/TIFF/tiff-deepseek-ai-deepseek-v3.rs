use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

#[derive(Debug)]
struct TiffHeader {
    byte_order: u16,
    version: u16,
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
    let (input, (byte_order, version, ifd_offset)) = tuple((le_u16, le_u16, le_u32))(input)?;
    Ok((
        input,
        TiffHeader {
            byte_order,
            version,
            ifd_offset,
        },
    ))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) =
        tuple((le_u16, le_u16, le_u32, le_u32))(input)?;
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

fn parse_ifd(input: &[u8], count: u16) -> IResult<&[u8], Vec<IfdEntry>> {
    let (mut input, mut entries) = (input, Vec::new());
    for _ in 0..count {
        let (new_input, entry) = parse_ifd_entry(input)?;
        entries.push(entry);
        input = new_input;
    }
    Ok((input, entries))
}

fn parse_tiff_file(input: &[u8]) -> IResult<&[u8], TiffFile> {
    let (input, header) = parse_tiff_header(input)?;
    let ifd_start = header.ifd_offset as usize;
    let (input, _) = take(ifd_start - 8)(input)?; // Skip to IFD
    let (input, ifd_count) = le_u16(input)?;
    let (input, ifd_entries) = parse_ifd(input, ifd_count)?;
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
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        return;
    }

    let file_data = fs::read(&args[1]).expect("Failed to read file");
    let result = parse_tiff_file(&file_data);

    match result {
        Ok((_, tiff_file)) => {
            println!("{:?}", tiff_file);
        }
        Err(e) => {
            eprintln!("Failed to parse TIFF file: {:?}", e);
        }
    }
}