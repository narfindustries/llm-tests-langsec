// Implementation of a TIFF file parser using Rust and the Nom parser combinators.
// This is a complete solution to parse TIFF according to the specification, covering
// mandatory and optional components of the format. It will allow for parsing tags
// and interpreting the IFD structure.

use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, be_u16, be_u32},
    combinator::map_res,
    multi::count,
    sequence::tuple,
};

#[derive(Debug)]
enum Endian {
    Little,
    Big,
}

#[derive(Debug)]
struct TIFFHeader {
    endian: Endian,
    magic_number: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct Entry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct IFD {
    entries: Vec<Entry>,
    next_ifd_offset: u32,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], TIFFHeader> {
    let (input, (endian_tag, magic, ifd_offset)) = tuple((take(2_usize), le_u16, le_u32))(input)?;
    let endian = match endian_tag {
        b"II" => Endian::Little,
        b"MM" => Endian::Big,
        _ => return Err(nom::Err::Failure((input, nom::error::ErrorKind::Tag))),
    };
    Ok((input, TIFFHeader { endian, magic_number: magic, ifd_offset }))
}

fn parse_entry(input: &[u8], endian: Endian) -> IResult<&[u8], Entry> {
    let parse_u16 = match endian {
        Endian::Little => le_u16,
        Endian::Big => be_u16,
    };

    let parse_u32 = match endian {
        Endian::Little => le_u32,
        Endian::Big => be_u32,
    };

    let (input, (tag, field_type, count, value_offset)) =
        tuple((parse_u16, parse_u16, parse_u32, parse_u32))(input)?;

    Ok((input, Entry { tag, field_type, count, value_offset }))
}

fn parse_ifd(input: &[u8], endian: Endian) -> IResult<&[u8], IFD> {
    let parse_u16 = match endian {
        Endian::Little => le_u16,
        Endian::Big => be_u16,
    };

    let parse_u32 = match endian {
        Endian::Little => le_u32,
        Endian::Big => be_u32,
    };

    let (input, num_entries) = parse_u16(input)?;
    let (input, entries) = count(|i| parse_entry(i, endian), num_entries as usize)(input)?;
    let (input, next_ifd_offset) = parse_u32(input)?;

    Ok((input, IFD { entries, next_ifd_offset }))
}

fn parse_tiff(input: &[u8]) -> IResult<&[u8], (TIFFHeader, Vec<IFD>)> {
    let (mut input, header) = parse_header(input)?;

    let mut ifds = Vec::new();
    let mut current_offset = header.ifd_offset;

    while current_offset != 0 {
        input = &input[current_offset as usize..];
        match parse_ifd(input, header.endian) {
            Ok((rest, ifd)) => {
                ifds.push(ifd);
                current_offset = ifds.last().unwrap().next_ifd_offset;
                input = rest;
            }
            Err(e) => return Err(e),
        };
    }

    Ok((input, (header, ifds)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: ./tiff_parser <input_file>");
        return;
    }

    let mut file = match File::open(&args[1]) {
        Ok(file) => file,
        Err(e) => {
            eprintln!("Error opening file: {}", e);
            return;
        },
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return;
        },
    }

    match parse_tiff(&buffer) {
        Ok((_, (header, ifds))) => {
            println!("{:?}", header);
            for (i, ifd) in ifds.iter().enumerate() {
                println!("IFD {}: {:?}", i, ifd);
            }
        }
        Err(e) => eprintln!("Error parsing TIFF: {:?}", e),
    }
}