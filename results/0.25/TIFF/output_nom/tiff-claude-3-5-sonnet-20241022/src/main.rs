use nom::{
    bytes::complete::{tag, take},
    combinator::{map, verify},
    multi::many0,
    number::complete::{le_u16, le_u32, le_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs::File, io::Read};

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
    ifds: Vec<Vec<IfdEntry>>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    map(
        tuple((
            verify(le_u16, |&x| x == 0x4949),
            verify(le_u16, |&x| x == 42),
            le_u32,
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
    let mut entries = Vec::new();
    let mut current_input = input;

    for _ in 0..entry_count {
        let (new_input, entry) = parse_ifd_entry(current_input)?;
        entries.push(entry);
        current_input = new_input;
    }

    let (remaining, _next_ifd_offset) = le_u32(current_input)?;
    Ok((remaining, entries))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], Tiff> {
    let (mut input, header) = parse_header(input)?;
    let mut ifds = Vec::new();
    
    // Skip to first IFD
    input = &input[header.ifd_offset as usize - 8..];
    
    loop {
        match parse_ifd(input) {
            Ok((new_input, ifd)) => {
                ifds.push(ifd);
                if new_input.len() < 4 {
                    break;
                }
                input = new_input;
            }
            Err(_) => break,
        }
    }

    Ok((input, Tiff { header, ifds }))
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
        Ok((_, tiff)) => println!("{:#?}", tiff),
        Err(e) => eprintln!("Failed to parse TIFF: {:?}", e),
    }

    Ok(())
}