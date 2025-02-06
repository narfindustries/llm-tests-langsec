use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::env;

#[derive(Debug)]
struct TiffHeader {
    byte_order: [u8; 2],
    version: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct IfdEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: [u8; 4],
}

#[derive(Debug)]
struct Ifd {
    num_entries: u16,
    entries: Vec<IfdEntry>,
    next_ifd_offset: u32,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = take(2usize)(input)?;
    let (input, version) = le_u16(input)?;
    let (input, ifd_offset) = le_u32(input)?;
    Ok((
        input,
        TiffHeader {
            byte_order: [byte_order[0], byte_order[1]],
            version,
            ifd_offset,
        },
    ))
}

fn parse_ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, (tag, field_type, count, value_offset)) =
        tuple((le_u16, le_u16, le_u32, take(4usize)))(input)?;
    Ok((
        input,
        IfdEntry {
            tag,
            field_type,
            count,
            value_offset: [value_offset[0], value_offset[1], value_offset[2], value_offset[3]],
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
            num_entries,
            entries,
            next_ifd_offset,
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

    let (input, header) = parse_tiff_header(&buffer).expect("Failed to parse TIFF header");
    println!("{:?}", header);

    let mut ifd_offset = header.ifd_offset;
    while ifd_offset != 0 {
        let (input, ifd) = parse_ifd(&buffer[ifd_offset as usize..]).expect("Failed to parse IFD");
        println!("{:?}", ifd);
        ifd_offset = ifd.next_ifd_offset;
    }
}