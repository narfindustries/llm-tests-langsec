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
struct IFD {
    entries: Vec<IFDEntry>,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = tag(b"II")(input)?;
    let (input, magic_number) = le_u16(input)?;
    let (input, ifd_offset) = le_u32(input)?;

    Ok((
        input,
        TiffHeader {
            byte_order: "II".to_string(),
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
    let (input, count) = le_u16(&input[offset as usize..])?;
    let mut entries = Vec::new();
    let mut entry_input = &input[(offset as usize + 2)..];

    for _ in 0..count {
        let (entry_input_new, entry) = parse_ifd_entry(entry_input)?;
        entry_input = entry_input_new;
        entries.push(entry);
    }

    Ok((entry_input, IFD { entries }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <TIFF_FILE>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff_header(&buffer) {
        Ok((input, header)) => {
            println!("Parsed TIFF Header: {:?}", header);
            match parse_ifd(input, header.ifd_offset) {
                Ok((_, ifd)) => {
                    println!("Parsed IFD: {:?}", ifd);
                }
                Err(e) => eprintln!("Failed to parse IFD: {:?}", e),
            }
        }
        Err(e) => eprintln!("Failed to parse TIFF header: {:?}", e),
    }

    Ok(())
}