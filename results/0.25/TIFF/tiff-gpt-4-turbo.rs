use nom::{
    bytes::complete::{take, take_while},
    combinator::{map, map_res},
    multi::many0,
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;

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
struct IFD {
    entries: Vec<IFDEntry>,
}

fn parse_byte_order(input: &[u8]) -> IResult<&[u8], String> {
    map(take(2usize), |bytes: &[u8]| match bytes {
        b"II" => "Little endian".to_string(),
        b"MM" => "Big endian".to_string(),
        _ => "Unknown".to_string(),
    })(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = parse_byte_order(input)?;
    let (input, magic_number) = le_u16(input)?;
    let (input, ifd_offset) = le_u32(input)?;
    Ok((
        input,
        TiffHeader {
            byte_order,
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

fn parse_ifd(input: &[u8], offset: u32) -> IResult<&[u8], IFD> {
    let (input, entry_count) = le_u16(&input[offset as usize..])?;
    let mut entries = Vec::new();
    let mut current_input = &input[(offset as usize + 2)..];
    for _ in 0..entry_count {
        let (new_input, entry) = parse_ifd_entry(current_input)?;
        entries.push(entry);
        current_input = new_input;
    }
    Ok((current_input, IFD { entries }))
}

fn read_file<P: AsRef<Path>>(path: P) -> io::Result<Vec<u8>> {
    let mut file = File::open(path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <TIFF_FILE>", args[0]);
        std::process::exit(1);
    }
    let file_path = &args[1];
    match read_file(file_path) {
        Ok(buffer) => {
            match parse_header(&buffer) {
                Ok((input, header)) => {
                    println!("Header: {:?}", header);
                    match parse_ifd(input, header.ifd_offset) {
                        Ok((_, ifd)) => {
                            println!("IFD: {:?}", ifd);
                        }
                        Err(e) => eprintln!("Failed to parse IFD: {:?}", e),
                    }
                }
                Err(e) => eprintln!("Failed to parse header: {:?}", e),
            }
        }
        Err(e) => eprintln!("Failed to read file: {:?}", e),
    }
}