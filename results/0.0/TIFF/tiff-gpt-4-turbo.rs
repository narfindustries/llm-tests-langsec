use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct TiffHeader {
    byte_order: String,
    magic_number: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct IFDEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct TIFF {
    header: TiffHeader,
    ifd_entries: Vec<IFDEntry>,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = tag(b"II")(input)?;
    let (input, magic_number) = le_u16(input)?;
    let (input, ifd_offset) = le_u32(input)?;

    Ok((
        input,
        TiffHeader {
            byte_order: String::from_utf8_lossy(byte_order).to_string(),
            magic_number,
            ifd_offset,
        },
    ))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IFDEntry> {
    let (input, tag) = le_u16(input)?;
    let (input, field_type) = le_u16(input)?;
    let (input, count) = le_u32(input)?;
    let (input, value_offset) = le_u32(input)?;

    Ok((
        input,
        IFDEntry {
            tag,
            field_type,
            count,
            value_offset,
        },
    ))
}

fn parse_ifd_entries(input: &[u8], num_entries: usize) -> IResult<&[u8], Vec<IFDEntry>> {
    let mut entries = Vec::new();
    let mut remaining = input;

    for _ in 0..num_entries {
        let (next_input, entry) = parse_ifd_entry(remaining)?;
        entries.push(entry);
        remaining = next_input;
    }

    Ok((remaining, entries))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], TIFF> {
    let (input, header) = parse_tiff_header(input)?;
    let (_, ifd_entries) = parse_ifd_entries(&input[(header.ifd_offset as usize)..], 12)?;

    Ok((input, TIFF { header, ifd_entries }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <TIFF_FILE>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff(&buffer) {
        Ok((_, tiff)) => println!("{:#?}", tiff),
        Err(e) => eprintln!("Failed to parse TIFF: {:?}", e),
    }

    Ok(())
}