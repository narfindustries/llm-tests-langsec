use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;
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
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct GraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct Block {
    image_descriptor: Option<ImageDescriptor>,
    graphic_control_extension: Option<GraphicControlExtension>,
    image_data: Option<Vec<u8>>,
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    blocks: Vec<Block>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, (signature, version)) = tuple((take(3usize), take(3usize)))(input)?;
    Ok((
        input,
        GifHeader {
            signature: String::from_utf8_lossy(signature).to_string(),
            version: String::from_utf8_lossy(version).to_string(),
        },
    ))
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

fn parse_color_table_entry(input: &[u8]) -> IResult<&[u8], ColorTableEntry> {
    let (input, (red, green, blue)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, ColorTableEntry { red, green, blue }))
}

fn parse_global_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    count(parse_color_table_entry, size)(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (_, left, top, width, height, packed_fields)) =
        tuple((tag(b","), le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
    Ok((
        input,
        ImageDescriptor {
            left,
            top,
            width,
            height,
            packed_fields,
        },
    ))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, (_, _, packed_fields, delay_time, transparent_color_index, _)) = tuple((
        tag(b"\x21\xF9"),
        le_u8,
        le_u8,
        le_u16,
        le_u8,
        le_u8,
    ))(input)?;
    Ok((
        input,
        GraphicControlExtension {
            packed_fields,
            delay_time,
            transparent_color_index,
        },
    ))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, lzw_minimum_code_size) = le_u8(input)?;
    let (input, blocks) = many0(preceded(le_u8, take(lzw_minimum_code_size as usize)))(input)?;
    let image_data: Vec<u8> = blocks.into_iter().flatten().collect();
    Ok((input, image_data))
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, graphic_control_extension) = opt(parse_graphic_control_extension)(input)?;
    let (input, image_descriptor) = opt(parse_image_descriptor)(input)?;
    let (input, image_data) = if image_descriptor.is_some() {
        let (input, data) = parse_image_data(input)?;
        (input, Some(data))
    } else {
        (input, None)
    };
    Ok((
        input,
        Block {
            image_descriptor,
            graphic_control_extension,
            image_data,
        },
    ))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let global_color_table_flag = (logical_screen_descriptor.packed_fields & 0b1000_0000) != 0;
    let global_color_table_size = if global_color_table_flag {
        2usize.pow(((logical_screen_descriptor.packed_fields & 0b0000_0111) + 1) as u32)
    } else {
        0
    };

    let (input, global_color_table) = if global_color_table_flag {
        let (input, table) = parse_global_color_table(input, global_color_table_size)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, blocks) = many0(parse_block)(input)?;

    Ok((
        input,
        Gif {
            header,
            logical_screen_descriptor,
            global_color_table,
            blocks,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_gif(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }
}