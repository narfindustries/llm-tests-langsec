use nom::{
    bytes::complete::take,
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;

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
    value_offset: u32,
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
    let (input, tag) = le_u16(input)?;
    let (input, field_type) = le_u16(input)?;
    let (input, count) = le_u32(input)?;
    let (input, value_offset) = le_u32(input)?;
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
    let mut input = input;
    for _ in 0..count {
        let (new_input, entry) = parse_ifd_entry(input)?;
        entries.push(entry);
        input = new_input;
    }
    Ok((input, entries))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    if let Err(err) = file.read_to_end(&mut buffer) {
        eprintln!("Error reading file: {}", err);
        return;
    }

    let (_, header) = match parse_tiff_header(&buffer) {
        Ok(result) => result,
        Err(err) => {
            eprintln!("Error parsing TIFF header: {:?}", err);
            return;
        }
    };

    println!("TIFF Header: {:?}", header);

    let ifd_offset = header.ifd_offset as usize;
    if ifd_offset >= buffer.len() {
        eprintln!("Invalid IFD offset");
        return;
    }

    let (_, entry_count) = match le_u16::<_, nom::error::Error<&[u8]>>(&buffer[ifd_offset..]) {
        Ok(result) => result,
        Err(err) => {
            eprintln!("Error parsing IFD entry count: {:?}", err);
            return;
        }
    };

    let (_, entries) = match parse_ifd_entries(&buffer[ifd_offset + 2..], entry_count) {
        Ok(result) => result,
        Err(err) => {
            eprintln!("Error parsing IFD entries: {:?}", err);
            return;
        }
    };

    for entry in entries {
        println!("IFD Entry: {:?}", entry);
    }
}