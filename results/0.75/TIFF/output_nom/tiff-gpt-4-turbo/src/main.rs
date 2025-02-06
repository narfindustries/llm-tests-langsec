use nom::{
    bytes::complete::take,
    combinator::map,
    multi::many_m_n,
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct TiffHeader {
    byte_order: u16,
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
struct TiffFile {
    header: TiffHeader,
    ifds: Vec<IfdEntry>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    map(
        tuple((le_u16, le_u16, le_u32)),
        |(byte_order, magic_number, ifd_offset)| TiffHeader {
            byte_order,
            magic_number,
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

fn parse_ifds(input: &[u8], offset: u32) -> IResult<&[u8], Vec<IfdEntry>> {
    let (input, _) = take(offset as usize)(input)?;
    let (input, count) = le_u16(input)?;
    many_m_n(count as usize, count as usize, parse_ifd_entry)(input)
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], TiffFile> {
    let (input, header) = parse_header(input)?;
    let (input, ifds) = parse_ifds(input, header.ifd_offset)?;
    Ok((input, TiffFile { header, ifds }))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_tiff(&buffer) {
        Ok((_remainder, tiff_file)) => {
            println!("{:#?}", tiff_file);
        }
        Err(err) => {
            eprintln!("Failed to parse TIFF: {:?}", err);
        }
    }

    Ok(())
}