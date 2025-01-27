use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, path::Path};

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
    ifd_entries: Vec<IfdEntry>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    map(
        tuple((
            verify(le_u16, |&x| x == 0x4949), // Little-endian
            verify(le_u16, |&x| x == 42),      // Version
            le_u32,                            // IFD offset
        )),
        |(byte_order, version, ifd_offset)| TiffHeader {
            byte_order,
            version,
            ifd_offset,
        },
    )(input)
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    map(
        tuple((le_u16, le_u16, le_u32, le_u32)),
        |(tag, field_type, count, value_offset)| IfdEntry {
            tag,
            field_type,
            count,
            value_offset,
        },
    )(input)
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], Vec<IfdEntry>> {
    let (input, entry_count) = le_u16(input)?;
    let mut entries = Vec::with_capacity(entry_count as usize);
    
    let mut current_input = input;
    for _ in 0..entry_count {
        let (new_input, entry) = parse_ifd_entry(current_input)?;
        entries.push(entry);
        current_input = new_input;
    }
    
    Ok((current_input, entries))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], Tiff> {
    let (input, header) = parse_header(input)?;
    let offset = header.ifd_offset as usize;
    let (_, ifd_entries) = parse_ifd(&input[offset..])?;
    
    Ok((input, Tiff {
        header,
        ifd_entries,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let data = fs::read(path)?;
    
    match parse_tiff(&data) {
        Ok((_, tiff)) => println!("{:#?}", tiff),
        Err(e) => eprintln!("Failed to parse TIFF: {:?}", e),
    }

    Ok(())
}