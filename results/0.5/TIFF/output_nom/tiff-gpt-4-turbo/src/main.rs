use nom::{
    bytes::complete::{take, tag},
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct TiffHeader {
    byte_order: String,
    magic_number: u16,
    offset: u32,
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
    let (input, (byte_order, magic_number, offset)) = tuple((tag(b"II"), le_u16, le_u32))(input)?;
    let byte_order = if byte_order == b"II" { "Little Endian" } else { "Big Endian" };
    Ok((input, TiffHeader { byte_order: byte_order.to_string(), magic_number, offset }))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IFDEntry> {
    let (input, (tag, field_type, count, value_offset)) = tuple((le_u16, le_u16, le_u32, le_u32))(input)?;
    Ok((input, IFDEntry { tag, field_type, count, value_offset }))
}

fn parse_ifd_entries(input: &[u8], num_entries: usize) -> IResult<&[u8], Vec<IFDEntry>> {
    let mut entries = Vec::new();
    let mut remaining = input;
    for _ in 0..num_entries {
        let (input_next, entry) = parse_ifd_entry(remaining)?;
        entries.push(entry);
        remaining = input_next;
    }
    Ok((remaining, entries))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], TIFF> {
    let (input, header) = parse_tiff_header(input)?;
    let num_ifd_entries = (input.len() - header.offset as usize) / 12; // Each IFD entry is 12 bytes
    let ifd_entries_input = &input[(header.offset as usize)..];
    let (_, ifd_entries) = parse_ifd_entries(ifd_entries_input, num_ifd_entries)?;

    Ok((input, TIFF { header, ifd_entries }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <TIFF_FILE_PATH>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff(&buffer) {
        Ok((_, tiff)) => println!("{:#?}", tiff),
        Err(e) => eprintln!("Failed to parse TIFF: {:?}", e),
    }

    Ok(())
}