use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    bytes::complete::{tag, take},
    multi::{count, many0},
    number::complete::{le_u8, le_u16},
    sequence::{tuple, preceded},
    combinator::{opt, map},
    branch::alt,
    IResult
};

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
struct GlobalColorTable {
    entries: Vec<ColorTableEntry>,
}

#[derive(Debug)]
struct ImageDescriptor {
    left_position: u16,
    top_position: u16,
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
enum ExtensionBlock {
    GraphicControl(GraphicControlExtension),
    Comment(Vec<u8>),
    PlainText,
    Application,
}

#[derive(Debug)]
struct GraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<GlobalColorTable>,
    image_descriptors: Vec<ImageDescriptor>,
    image_data: Vec<ImageData>,
    extensions: Vec<ExtensionBlock>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, (signature, version)) = tuple((
        take(3usize),
        take(3usize)
    ))(input)?;

    Ok((input, GifHeader {
        signature: signature.try_into().unwrap(),
        version: version.try_into().unwrap(),
    }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) = tuple((
        le_u16,
        le_u16,
        le_u8,
        le_u8,
        le_u8
    ))(input)?;

    Ok((input, LogicalScreenDescriptor {
        width,
        height,
        packed_fields,
        background_color_index,
        pixel_aspect_ratio,
    }))
}

fn parse_color_table_entry(input: &[u8]) -> IResult<&[u8], ColorTableEntry> {
    let (input, (red, green, blue)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, ColorTableEntry { red, green, blue }))
}

fn parse_global_color_table(input: &[u8], size: usize) -> IResult<&[u8], GlobalColorTable> {
    let (input, entries) = count(parse_color_table_entry, size)(input)?;
    Ok((input, GlobalColorTable { entries }))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag([0x2C])(input)?;
    let (input, (left_position, top_position, width, height, packed_fields)) = tuple((
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u8
    ))(input)?;

    Ok((input, ImageDescriptor {
        left_position,
        top_position,
        width,
        height,
        packed_fields,
    }))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], ImageData> {
    let (input, lzw_min_code_size) = le_u8(input)?;
    let (input, data_blocks) = many0(parse_data_block)(input)?;

    Ok((input, ImageData {
        lzw_min_code_size,
        data_blocks,
    }))
}

fn parse_data_block(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, size) = le_u8(input)?;
    let (input, block) = take(size as usize)(input)?;
    Ok((input, block.to_vec()))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag([0x21, 0xF9])(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, (packed_fields, delay_time, transparent_color_index)) = tuple((
        le_u8,
        le_u16,
        le_u8
    ))(input)?;
    let (input, _) = tag([0x00])(input)?;

    Ok((input, GraphicControlExtension {
        packed_fields,
        delay_time,
        transparent_color_index,
    }))
}

fn parse_extension_block(input: &[u8]) -> IResult<&[u8], ExtensionBlock> {
    let (input, _) = tag([0x21])(input)?;
    alt((
        map(parse_graphic_control_extension, ExtensionBlock::GraphicControl),
        map(parse_comment_extension, ExtensionBlock::Comment),
        map(tag([0x01]), |_| ExtensionBlock::PlainText),
        map(tag([0xFF]), |_| ExtensionBlock::Application)
    ))(input)
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag([0xFE])(input)?;
    let (input, blocks) = many0(parse_data_block)(input)?;
    Ok((input, blocks.into_iter().flatten().collect()))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let global_color_table = if logical_screen_descriptor.packed_fields & 0x80 != 0 {
        let table_size = 2 << (logical_screen_descriptor.packed_fields & 0x07);
        let (input, global_color_table) = parse_global_color_table(input, table_size)?;
        Some(global_color_table)
    } else {
        None
    };

    let (mut input, mut image_descriptors) = many0(parse_image_descriptor)(input)?;
    let mut image_data = Vec::new();
    let mut extensions = Vec::new();

    for _ in 0..image_descriptors.len() {
        let (remaining, img_data) = parse_image_data(input)?;
        image_data.push(img_data);
        input = remaining;
    }

    while let Ok((remaining, extension)) = parse_extension_block(input) {
        extensions.push(extension);
        input = remaining;
    }

    let (input, _) = tag([0x3B])(input)?;

    Ok((input, Gif {
        header,
        logical_screen_descriptor,
        global_color_table,
        image_descriptors,
        image_data,
        extensions,
    }))
}

fn main() -> std::io::Result<()> {
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
            println!("Successfully parsed GIF: {:?}", gif);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse GIF: {:?}", e);
            std::process::exit(1);
        }
    }
}