use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    IResult,
    bytes::complete::take,
    number::complete::{
        le_u16, le_u32, be_u16, be_u32
    },
    multi::count,
    combinator::opt,
};

#[derive(Debug, Clone, Copy)]
enum Endianness {
    LittleEndian,
    BigEndian,
}

#[derive(Debug)]
struct TiffHeader {
    endianness: Endianness,
    magic_number: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct IFDEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_or_offset: u32,
}

#[derive(Debug)]
struct ImageFileDirectory {
    entries: Vec<IFDEntry>,
    next_ifd_offset: Option<u32>,
}

#[derive(Debug)]
struct TiffFile {
    header: TiffHeader,
    ifds: Vec<ImageFileDirectory>,
}

fn parse_endianness(input: &[u8]) -> IResult<&[u8], Endianness> {
    let (input, endian_bytes) = take(2usize)(input)?;
    match endian_bytes {
        b"II" => Ok((input, Endianness::LittleEndian)),
        b"MM" => Ok((input, Endianness::BigEndian)),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_magic_number(endianness: Endianness) -> impl Fn(&[u8]) -> IResult<&[u8], u16> {
    move |input| {
        match endianness {
            Endianness::LittleEndian => le_u16(input),
            Endianness::BigEndian => be_u16(input),
        }
    }
}

fn parse_u32(endianness: Endianness) -> impl Fn(&[u8]) -> IResult<&[u8], u32> {
    move |input| {
        match endianness {
            Endianness::LittleEndian => le_u32(input),
            Endianness::BigEndian => be_u32(input),
        }
    }
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, endianness) = parse_endianness(input)?;
    let parse_magic = parse_magic_number(endianness);
    let parse_u32 = parse_u32(endianness);

    let (input, magic_number) = parse_magic(input)?;
    let (input, ifd_offset) = parse_u32(input)?;

    Ok((input, TiffHeader {
        endianness,
        magic_number,
        ifd_offset,
    }))
}

fn parse_ifd_entry(endianness: Endianness) -> impl Fn(&[u8]) -> IResult<&[u8], IFDEntry> {
    move |input| {
        let parse_u16 = match endianness {
            Endianness::LittleEndian => le_u16,
            Endianness::BigEndian => be_u16,
        };
        let parse_u32 = match endianness {
            Endianness::LittleEndian => le_u32,
            Endianness::BigEndian => be_u32,
        };

        let (input, tag) = parse_u16(input)?;
        let (input, field_type) = parse_u16(input)?;
        let (input, count) = parse_u32(input)?;
        let (input, value_or_offset) = parse_u32(input)?;

        Ok((input, IFDEntry {
            tag,
            field_type,
            count,
            value_or_offset,
        }))
    }
}

fn parse_image_file_directory(endianness: Endianness) -> impl Fn(&[u8]) -> IResult<&[u8], ImageFileDirectory> {
    move |input| {
        let parse_u16 = match endianness {
            Endianness::LittleEndian => le_u16,
            Endianness::BigEndian => be_u16,
        };

        let (input, num_entries) = parse_u16(input)?;
        let (input, entries) = count(parse_ifd_entry(endianness), num_entries as usize)(input)?;
        
        let parse_u32 = parse_u32(endianness);
        let (input, next_ifd_offset) = opt(parse_u32)(input)?;

        Ok((input, ImageFileDirectory {
            entries,
            next_ifd_offset,
        }))
    }
}

fn parse_tiff_file(input: &[u8]) -> IResult<&[u8], TiffFile> {
    let (input, header) = parse_tiff_header(input)?;
    
    let mut ifds = Vec::new();
    let mut current_input = &input[header.ifd_offset as usize..];
    
    loop {
        let (current_remaining, ifd) = parse_image_file_directory(header.endianness)(current_input)?;
        ifds.push(ifd);
        
        match ifds.last().unwrap().next_ifd_offset {
            Some(offset) => current_input = &current_input[offset as usize..],
            None => break,
        }
    }

    Ok((input, TiffFile {
        header,
        ifds,
    }))
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

    match parse_tiff_file(&buffer) {
        Ok((_, tiff_file)) => {
            println!("Parsed TIFF File: {:?}", tiff_file);
            Ok(())
        },
        Err(e) => {
            eprintln!("Failed to parse TIFF file: {:?}", e);
            std::process::exit(1);
        }
    }
}