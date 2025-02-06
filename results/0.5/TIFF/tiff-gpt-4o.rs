use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32},
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

#[derive(Debug)]
struct TiffHeader {
    byte_order: [u8; 2],
    version: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct TiffTag {
    tag: u16,
    field_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
struct TiffIFD {
    num_tags: u16,
    tags: Vec<TiffTag>,
    next_ifd_offset: u32,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = take(2usize)(input)?;
    let (input, version) = le_u16(input)?;
    let (input, ifd_offset) = le_u32(input)?;
    Ok((input, TiffHeader { byte_order: [byte_order[0], byte_order[1]], version, ifd_offset }))
}

fn parse_tiff_tag(input: &[u8]) -> IResult<&[u8], TiffTag> {
    let (input, tag) = le_u16(input)?;
    let (input, field_type) = le_u16(input)?;
    let (input, count) = le_u32(input)?;
    let (input, value_offset) = le_u32(input)?;
    Ok((input, TiffTag { tag, field_type, count, value_offset }))
}

fn parse_tiff_ifd(input: &[u8]) -> IResult<&[u8], TiffIFD> {
    let (input, num_tags) = le_u16(input)?;
    let mut input = input;
    let mut tags = Vec::new();
    for _ in 0..num_tags {
        let (new_input, tag) = parse_tiff_tag(input)?;
        input = new_input;
        tags.push(tag);
    }
    let (input, next_ifd_offset) = le_u32(input)?;
    Ok((input, TiffIFD { num_tags, tags, next_ifd_offset }))
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

    match parse_tiff_header(&buffer) {
        Ok((remaining, header)) => {
            println!("TIFF Header: {:?}", header);
            let mut offset = header.ifd_offset as usize;
            while offset != 0 {
                match parse_tiff_ifd(&buffer[offset..]) {
                    Ok((_, ifd)) => {
                        println!("IFD: {:?}", ifd);
                        offset = ifd.next_ifd_offset as usize;
                    }
                    Err(err) => {
                        eprintln!("Error parsing IFD: {:?}", err);
                        break;
                    }
                }
            }
        }
        Err(err) => eprintln!("Error parsing TIFF header: {:?}", err),
    }
}