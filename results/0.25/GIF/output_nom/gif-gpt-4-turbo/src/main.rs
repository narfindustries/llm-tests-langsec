use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{count, many0},
    number::complete::{le_u16, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::env;

#[derive(Debug)]
struct GifHeader {
    signature: String,
    version: String,
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
    image_left: u16,
    image_top: u16,
    image_width: u16,
    image_height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct GifData {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    image_descriptor: ImageDescriptor,
    image_data: Vec<u8>,
}

fn parse_color_table_entry(input: &[u8]) -> IResult<&[u8], ColorTableEntry> {
    map(tuple((le_u8, le_u8, le_u8)), |(red, green, blue)| ColorTableEntry { red, green, blue })(input)
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    map(
        tuple((tag("GIF"), take(3usize))),
        |(_, version): (_, &[u8])| GifHeader {
            signature: "GIF".to_string(),
            version: String::from_utf8(version.to_vec()).unwrap(),
        },
    )(input)
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    map(
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8)),
        |(width, height, packed_fields, background_color_index, pixel_aspect_ratio)| LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
            background_color_index,
            pixel_aspect_ratio,
        },
    )(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    map(
        tuple((tag(","), le_u16, le_u16, le_u16, le_u16, le_u8)),
        |(_, image_left, image_top, image_width, image_height, packed_fields)| ImageDescriptor {
            image_left,
            image_top,
            image_width,
            image_height,
            packed_fields,
        },
    )(input)
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GifData> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let global_color_table_flag = (logical_screen_descriptor.packed_fields & 0b10000000) > 0;
    let global_color_table_size = if global_color_table_flag {
        1 << ((logical_screen_descriptor.packed_fields & 0b00000111) + 1)
    } else {
        0
    };

    let (input, global_color_table) = if global_color_table_flag {
        map(count(parse_color_table_entry, global_color_table_size as usize), Some)(input)?
    } else {
        (input, None)
    };

    let (input, image_descriptor) = parse_image_descriptor(input)?;
    let (input, image_data) = preceded(tag(","), many0(le_u8))(input)?;

    Ok((
        input,
        GifData {
            header,
            logical_screen_descriptor,
            global_color_table,
            image_descriptor,
            image_data,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No input file specified"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => println!("Failed to parse GIF: {:?}", e),
    }

    Ok(())
}