use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u64},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs;

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
struct Ifd {
    num_entries: u16,
    entries: Vec<IfdEntry>,
    next_ifd_offset: u32,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, (byte_order, magic, ifd_offset)) = tuple((le_u16, le_u16, le_u32))(input)?;
    Ok((input, TiffHeader { byte_order, magic, ifd_offset }))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) = tuple((le_u16, le_u16, le_u32, le_u32))(input)?;
    Ok((input, IfdEntry { tag, field_type, count, value_offset }))
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], Ifd> {
    let (input, num_entries) = le_u16(input)?;
    let (input, entries) = nom::multi::count(parse_ifd_entry, num_entries as usize)(input)?;
    let (input, next_ifd_offset) = le_u32(input)?;
    Ok((input, Ifd { num_entries, entries, next_ifd_offset }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <tiff_file>", args[0]);
        return;
    }

    let file_path = &args[1];
    let data = fs::read(file_path).expect("Failed to read file");

    let (remaining, header) = parse_tiff_header(&data).expect("Failed to parse TIFF header");
    println!("{:?}", header);

    let mut offset = header.ifd_offset as usize;
    loop {
        if offset >= data.len() {
            break;
        }

        let (remaining, ifd) = parse_ifd(&data[offset..]).expect("Failed to parse IFD");
        println!("{:?}", ifd);

        if ifd.next_ifd_offset == 0 {
            break;
        }
        offset = ifd.next_ifd_offset as usize;
    }
}