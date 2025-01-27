use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct TIFFHeader {
    endianess: u16,
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

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TIFFHeader> {
    let (input, endianess) = le_u16(input)?;
    let (input, magic_number) = le_u16(input)?;
    let (input, ifd_offset) = le_u32(input)?;
    Ok((
        input,
        TIFFHeader {
            endianess,
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

fn parse_ifd(input: &[u8], num_entries: u16) -> IResult<&[u8], IFD> {
    let (input, entries) = count(parse_ifd_entry, num_entries as usize)(input)?;
    Ok((input, IFD { entries }))
}

fn read_tiff_file(file_path: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(file_path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <FILE>", args[0]);
        std::process::exit(1);
    }

    let data = read_tiff_file(&args[1])?;

    match parse_tiff_header(&data) {
        Ok((input, header)) => {
            println!("TIFF Header: {:?}", header);
            let ifd_start = header.ifd_offset as usize;
            match parse_ifd(&data[ifd_start..], 1) { // assuming there's 1 IFD for simplicity
                Ok((_, ifd)) => {
                    println!("IFD: {:?}", ifd);
                }
                Err(e) => {
                    println!("Failed to parse IFD: {:?}", e);
                }
            }
        }
        Err(e) => {
            println!("Failed to parse TIFF header: {:?}", e);
        }
    }

    Ok(())
}