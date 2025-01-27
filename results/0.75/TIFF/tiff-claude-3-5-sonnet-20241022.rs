use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32, be_u16, be_u32},
    IResult, combinator::map,
    sequence::tuple,
    multi::count,
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
struct Tiff {
    header: TiffHeader,
    entries: Vec<IfdEntry>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, (byte_order, version, ifd_offset)) = tuple((
        le_u16,
        le_u16,
        le_u32,
    ))(input)?;

    Ok((input, TiffHeader {
        byte_order,
        version,
        ifd_offset,
    }))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) = tuple((
        le_u16,
        le_u16,
        le_u32,
        le_u32,
    ))(input)?;

    Ok((input, IfdEntry {
        tag,
        field_type,
        count,
        value_offset,
    }))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], Tiff> {
    let (input, header) = parse_header(input)?;
    let (input, num_entries) = le_u16(input)?;
    let (input, entries) = count(parse_ifd_entry, num_entries as usize)(input)?;

    Ok((input, Tiff {
        header,
        entries,
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

    match parse_tiff(&buffer) {
        Ok((remaining, tiff)) => {
            println!("Parsed TIFF: {:#?}", tiff);
            println!("Remaining bytes: {} bytes", remaining.len());
        }
        Err(e) => {
            eprintln!("Failed to parse TIFF: {:?}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}