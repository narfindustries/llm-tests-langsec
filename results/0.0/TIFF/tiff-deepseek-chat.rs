use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::tuple,
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
struct IfdEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct Ifd {
    entries: Vec<IfdEntry>,
    next_ifd_offset: u32,
}

#[derive(Debug)]
struct TiffFile {
    header: TiffHeader,
    ifds: Vec<Ifd>,
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

fn parse_ifd(input: &[u8]) -> IResult<&[u8], Ifd> {
    let (input, num_entries) = le_u16(input)?;
    let (input, entries) = nom::multi::count(parse_ifd_entry, num_entries as usize)(input)?;
    let (input, next_ifd_offset) = le_u32(input)?;
    Ok((
        input,
        Ifd {
            entries,
            next_ifd_offset,
        },
    ))
}

fn parse_tiff_file(input: &[u8]) -> IResult<&[u8], TiffFile> {
    let (input, header) = parse_tiff_header(input)?;
    let mut ifds = Vec::new();
    let mut next_ifd_offset = header.ifd_offset;

    while next_ifd_offset != 0 {
        let (remaining_input, ifd) = parse_ifd(&input[next_ifd_offset as usize..])?;
        next_ifd_offset = ifd.next_ifd_offset;
        ifds.push(ifd);
    }

    Ok((input, TiffFile { header, ifds }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_tiff_file(&buffer) {
        Ok((_, tiff_file)) => {
            println!("{:#?}", tiff_file);
        }
        Err(e) => {
            eprintln!("Failed to parse TIFF file: {:?}", e);
        }
    }
}