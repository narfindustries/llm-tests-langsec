use std::env;
use std::fs::File;
use std::io::Read;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    number::complete::{be_u16, be_u32},
    sequence::{preceded, tuple},
    IResult,
};

#[derive(Debug)]
struct TiffHeader {
    byte_order: [u8; 2],
    version: [u8; 2],
}

#[derive(Debug)]
struct IfdEntry {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct ImageFileDirectory {
    entries: Vec<IfdEntry>,
    next_ifd_offset: u32,
}


fn tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = take(2usize)(input)?;
    let (input, version) = take(2usize)(input)?;
    Ok((input, TiffHeader { byte_order: byte_order.try_into().unwrap(), version: version.try_into().unwrap() }))
}

fn ifd_entry(input: &[u8]) -> IResult<&[u8], IfdEntry> {
    let (input, tag) = be_u16(input)?;
    let (input, field_type) = be_u16(input)?;
    let (input, count) = be_u32(input)?;
    let (input, value_offset) = be_u32(input)?;
    Ok((input, IfdEntry { tag, field_type, count, value_offset }))
}

fn image_file_directory(input: &[u8]) -> IResult<&[u8], ImageFileDirectory> {
    let (input, num_entries) = be_u16(input)?;
    let (input, entries) =  nom::multi::count(ifd_entry, num_entries as usize)(input)?;
    let (input, next_ifd_offset) = be_u32(input)?;
    Ok((input, ImageFileDirectory { entries, next_ifd_offset }))
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <tiff_file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match tiff_header(&buffer) {
        Ok((remaining, header)) => {
            println!("TIFF Header: {:?}", header);
            match image_file_directory(remaining) {
                Ok((_, ifd)) => {
                    println!("IFD: {:?}", ifd);
                }
                Err(e) => println!("Error parsing IFD: {:?}", e),
            }

        }
        Err(e) => println!("Error parsing TIFF header: {:?}", e),
    }
}
