extern crate nom;

use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::count,
    number::complete::{le_u16, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

// Define structures for various parts of the GIF file
#[derive(Debug)]
struct GifHeader {
    signature: Vec<u8>,
    version: Vec<u8>,
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct ColorTable {
    colors: Vec<(u8, u8, u8)>, // RGB
}

#[derive(Debug)]
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct ImageData {
    lzw_minimum_code_size: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
enum GifBlock {
    Header(GifHeader),
    LogicalScreenDescriptor(LogicalScreenDescriptor),
    GlobalColorTable(ColorTable),
    ImageDescriptor(ImageDescriptor),
    ImageData(ImageData),
    Other(u8),
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = take(3u8)(input)?;
    let (input, version) = take(3u8)(input)?;
    Ok((input, GifHeader { signature: signature.to_vec(), version: version.to_vec() }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
            background_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], ColorTable> {
    let (input, colors) = count(tuple((le_u8, le_u8, le_u8)), size)(input)?;
    Ok((input, ColorTable { colors }))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (left, top, width, height, packed_fields)) =
        preceded(tag(&[0x2C]), tuple((le_u16, le_u16, le_u16, le_u16, le_u8)))(input)?;
    Ok((input, ImageDescriptor { left, top, width, height, packed_fields }))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], ImageData> {
    let (input, lzw_minimum_code_size) = le_u8(input)?;
    let (input, data) = parse_data_blocks(input)?;
    Ok((input, ImageData { lzw_minimum_code_size, data }))
}

fn parse_data_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut input = input;
    let mut data = Vec::new();
    loop {
        let (new_input, size) = le_u8(input)?;
        if size == 0 {
            break;
        }
        let (new_input, block_data) = take(size)(new_input)?;
        data.extend_from_slice(block_data);
        input = new_input;
    }
    Ok((input, data))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Vec<GifBlock>> {
    let (input, header) = parse_header(input)?;
    let (input, lsd) = parse_logical_screen_descriptor(input)?;

    let global_color_table_size = if (lsd.packed_fields & 0x80) != 0 {
        2_usize.pow(((lsd.packed_fields & 0x07) + 1) as u32)
    } else {
        0
    };
    let (input, global_color_table) = if global_color_table_size > 0 {
        let (input, gct) = parse_color_table(input, global_color_table_size)?;
        (input, Some(GifBlock::GlobalColorTable(gct)))
    } else {
        (input, None)
    };

    let mut remaining = input;
    let mut blocks = vec![
        GifBlock::Header(header),
        GifBlock::LogicalScreenDescriptor(lsd),
    ];

    if let Some(gct) = global_color_table {
        blocks.push(gct);
    }

    while !remaining.is_empty() {
        if remaining.len() < 1 {
            break;
        }
        match remaining[0] {
            0x2C => {
                let (input, descriptor) = parse_image_descriptor(remaining)?;
                let (input, data) = parse_image_data(input)?;
                blocks.push(GifBlock::ImageDescriptor(descriptor));
                blocks.push(GifBlock::ImageData(data));
                remaining = input;
            }
            0x3B => break, // Trailer
            _ => {
                let unknown_block = remaining[0];
                remaining = &remaining[1..];
                blocks.push(GifBlock::Other(unknown_block));
            }
        }
    }
    Ok((remaining, blocks))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_gif(&buffer) {
        Ok((_, blocks)) => {
            for block in blocks {
                println!("{:?}", block);
            }
        }
        Err(e) => eprintln!("Error parsing GIF: {:?}", e),
    }
}