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
    byte_order: u16,
    version: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
struct TiffTag {
    tag_id: u16,
    data_type: u16,
    count: u32,
    value_offset: u32,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = le_u16(input)?;
    let (input, version) = le_u16(input)?;
    let (input, ifd_offset) = le_u32(input)?;
    Ok((
        input,
        TiffHeader {
            byte_order,
            version,
            ifd_offset,
        },
    ))
}

fn parse_tiff_tag(input: &[u8]) -> IResult<&[u8], TiffTag> {
    let (input, tag_id) = le_u16(input)?;
    let (input, data_type) = le_u16(input)?;
    let (input, count) = le_u32(input)?;
    let (input, value_offset) = le_u32(input)?;
    Ok((
        input,
        TiffTag {
            tag_id,
            data_type,
            count,
            value_offset,
        },
    ))
}

fn parse_ifd(input: &[u8]) -> IResult<&[u8], Vec<TiffTag>> {
    let (input, num_tags) = le_u16(input)?;
    let mut tags = Vec::new();
    let mut input = input;
    for _ in 0..num_tags {
        let (new_input, tag) = parse_tiff_tag(input)?;
        tags.push(tag);
        input = new_input;
    }
    Ok((input, tags))
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
            if let Ok((_, tags)) = parse_ifd(&remaining[(header.ifd_offset as usize)..]) {
                for tag in tags {
                    println!("Tag: {:?}", tag);
                }
            } else {
                eprintln!("Error parsing IFD");
            }
        }
        Err(err) => eprintln!("Error parsing TIFF header: {:?}", err),
    }
}