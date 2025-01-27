use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
    path::Path,
};

#[derive(Debug)]
struct TiffHeader {
    byte_order: String,
    magic_number: u16,
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
    ifds: Vec<IfdEntry>,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = take(2usize)(input)?;
    let byte_order = match byte_order {
        b"II" => "Little endian",
        b"MM" => "Big endian",
        _ => return Err(nom::Err::Error((input, nom::error::ErrorKind::Tag))),
    };
    let (input, magic_number) = le_u16(input)?;
    let (input, ifd_offset) = le_u32(input)?;
    Ok((input, TiffHeader { byte_order: byte_order.to_string(), magic_number, ifd_offset }))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, tag) = le_u16(input)?;
    let (input, field_type) = le_u16(input)?;
    let (input, count) = le_u32(input)?;
    let (input, value_offset) = le_u32(input)?;
    Ok((input, IfdEntry { tag, field_type, count, value_offset }))
}

fn parse_ifds(input: &[u8], offset: u32, num_entries: usize) -> IResult<&[u8], Vec<IfdEntry>> {
    let (input, _) = take(offset as usize)(input)?;
    let mut input = input;
    let mut ifds = Vec::new();
    let (mut input, entry_count) = le_u16(input)?;
    for _ in 0..entry_count {
        let (new_input, ifd_entry) = parse_ifd_entry(input)?;
        ifds.push(ifd_entry);
        input = new_input;
    }
    Ok((input, ifds))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], Tiff>{
    let (input, header) = parse_tiff_header(input)?;
    match parse_ifds(input, header.ifd_offset, 0) {
        Ok((remaining, ifds)) => Ok((remaining, Tiff { header, ifds })),
        Err(e) => Err(e),
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <file>", args[0]);
        return Ok(());
    }
    let path = Path::new(&args[1]);
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    match parse_tiff(&buffer) {
        Ok((_remaining, tiff)) => println!("{:#?}", tiff),
        Err(error) => println!("Failed to parse TIFF: {:?}", error),
    }
    Ok(())
}