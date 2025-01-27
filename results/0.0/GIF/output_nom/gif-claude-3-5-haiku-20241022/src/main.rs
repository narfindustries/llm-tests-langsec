use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u8, le_u16, le_u32},
    sequence::{tuple, preceded},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 3],
    version: [u8; 3],
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
struct ColorTableEntry {
    red: u8,
    green: u8,
    blue: u8,
}

#[derive(Debug)]
struct ImageDescriptor {
    separator: u8,
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct ImageData {
    lzw_min_code_size: u8,
    data_blocks: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct GifFile {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    images: Vec<(ImageDescriptor, ImageData)>,
    extensions: Vec<Vec<u8>>,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    map(
        tuple((
            take(3usize),
            take(3usize),
        )),
        |(signature, version)| GifHeader {
            signature: signature.try_into().unwrap(),
            version: version.try_into().unwrap(),
        }
    )(input)
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    map(
        tuple((
            le_u16,
            le_u16,
            le_u8,
            le_u8,
            le_u8,
        )),
        |(width, height, packed_fields, background_color_index, pixel_aspect_ratio)| 
        LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
            background_color_index,
            pixel_aspect_ratio,
        }
    )(input)
}

fn parse_color_table(input: &[u8], color_table_size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    count(
        map(
            tuple((le_u8, le_u8, le_u8)),
            |(red, green, blue)| ColorTableEntry { red, green, blue }
        ),
        color_table_size
    )(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    map(
        tuple((
            tag(&[0x2C]),
            le_u16,
            le_u16,
            le_u16,
            le_u16,
            le_u8,
        )),
        |(separator, left, top, width, height, packed_fields)| 
        ImageDescriptor {
            separator,
            left,
            top,
            width,
            height,
            packed_fields,
        }
    )(input)
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], ImageData> {
    map(
        tuple((
            le_u8,
            many0(preceded(le_u8, take_while!(|x: u8| x != 0))),
        )),
        |(lzw_min_code_size, data_blocks)| ImageData {
            lzw_min_code_size,
            data_blocks,
        }
    )(input)
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GifFile> {
    let (input, header) = parse_gif_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let global_color_table_size = if logical_screen_descriptor.packed_fields & 0x80 != 0 {
        1 << ((logical_screen_descriptor.packed_fields & 0x07) + 1)
    } else {
        0
    };

    let (input, global_color_table) = if global_color_table_size > 0 {
        let (input, color_table) = parse_color_table(input, global_color_table_size)?;
        (input, Some(color_table))
    } else {
        (input, None)
    };

    let (mut input, mut images) = many0(
        tuple((
            parse_image_descriptor,
            parse_image_data,
        ))
    )(input)?;

    let (input, extensions) = many0(take_while!(|x: u8| x == 0x21))(input)?;

    Ok((input, GifFile {
        header,
        logical_screen_descriptor,
        global_color_table,
        images,
        extensions,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gif_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((_, gif)) => {
            println!("Parsed GIF: {:?}", gif);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse GIF: {:?}", e);
            std::process::exit(1);
        }
    }
}