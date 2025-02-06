extern crate nom;
use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u32, le_u8},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::env;

#[derive(Debug)]
pub struct TiffHeader {
    byte_order: [u8; 2],
    magic_number: u16,
    ifd_offset: u32,
}

#[derive(Debug)]
pub struct TiffTag {
    tag_id: u16,
    data_type: u16,
    count: u32,
    value_offset: u32,
}

#[derive(Debug)]
pub struct TiffIfd {
    num_tags: u16,
    tags: Vec<TiffTag>,
    next_ifd_offset: u32,
}

fn parse_tiff_header(input: &[u8]) -> IResult<&[u8], TiffHeader> {
    let (input, byte_order) = take(2usize)(input)?;
    let (input, magic_number) = le_u16(input)?;
    let (input, ifd_offset) = le_u32(input)?;
    Ok((
        input,
        TiffHeader {
            byte_order: [byte_order[0], byte_order[1]],
            magic_number,
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

fn parse_tiff_ifd(input: &[u8]) -> IResult<&[u8], TiffIfd> {
    let (input, num_tags) = le_u16(input)?;
    let mut tags = Vec::with_capacity(num_tags as usize);
    let mut input = input;
    for _ in 0..num_tags {
        let (new_input, tag) = parse_tiff_tag(input)?;
        input = new_input;
        tags.push(tag);
    }
    let (input, next_ifd_offset) = le_u32(input)?;
    Ok((input, TiffIfd { num_tags, tags, next_ifd_offset }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file.tiff>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Err(why) => panic!("Couldn't open {}: {}", path.display(), why),
        Ok(file) => file,
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Err(why) => panic!("Couldn't read {}: {}", path.display(), why),
        Ok(_) => (),
    }

    let (_, tiff_header) = parse_tiff_header(&buffer).expect("Failed to parse TIFF header");
    println!("{:?}", tiff_header);

    // Move to the first IFD offset
    let first_ifd_offset = tiff_header.ifd_offset as usize;
    let (_, tiff_ifd) = parse_tiff_ifd(&buffer[first_ifd_offset..]).expect("Failed to parse TIFF IFD");
    
    println!("{:?}", tiff_ifd);
}