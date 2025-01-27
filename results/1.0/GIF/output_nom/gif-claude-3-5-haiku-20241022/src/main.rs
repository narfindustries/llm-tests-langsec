use nom::{
    bits::{streaming::take as take_bits, streaming::tag as tag_bits},
    bytes::streaming::{tag, take},
    error::ErrorKind,
    multi::{count, many0, many_m_n},
    number::streaming::{le_u8, le_u16, le_u24},
    sequence::{tuple, preceded},
    IResult, Parser,
};
use std::{env, fs, process};

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
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct GraphicControlExtension {
    block_size: u8,
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
enum Block {
    ImageDescriptor(ImageDescriptor),
    GraphicControlExtension(GraphicControlExtension),
    PlainTextExtension,
    ApplicationExtension,
    CommentExtension,
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    blocks: Vec<Block>,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = take(3usize)(input)?;
    let (input, version) = take(3usize)(input)?;
    
    Ok((input, GifHeader {
        signature: signature.try_into().unwrap(),
        version: version.try_into().unwrap(),
    }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, background_color_index) = le_u8(input)?;
    let (input, pixel_aspect_ratio) = le_u8(input)?;

    Ok((input, LogicalScreenDescriptor {
        width,
        height,
        packed_fields,
        background_color_index,
        pixel_aspect_ratio,
    }))
}

fn parse_color_table(input: &[u8], color_table_size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    let (input, color_entries) = count(
        tuple((le_u8, le_u8, le_u8)).map(|(r, g, b)| ColorTableEntry { red: r, green: g, blue: b }),
        color_table_size
    )(input)?;

    Ok((input, color_entries))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _extension_introducer) = tag([0x21])(input)?;
    let (input, _graphic_control_label) = tag([0xF9])(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, delay_time) = le_u16(input)?;
    let (input, transparent_color_index) = le_u8(input)?;
    let (input, _block_terminator) = tag([0x00])(input)?;

    Ok((input, GraphicControlExtension {
        block_size,
        packed_fields,
        delay_time,
        transparent_color_index,
    }))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _separator) = tag([0x2C])(input)?;
    let (input, left) = le_u16(input)?;
    let (input, top) = le_u16(input)?;
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;

    Ok((input, ImageDescriptor {
        left,
        top,
        width,
        height,
        packed_fields,
    }))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_gif_header(input)?;
    
    if header.signature != *b"GIF" || ![b"87a", b"89a"].contains(&header.version) {
        return Err(nom::Err::Error(nom::error::Error::new(input, ErrorKind::Verify)));
    }

    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    
    let has_global_color_table = (logical_screen_descriptor.packed_fields & 0x80) != 0;
    let global_color_table_size = 1 << ((logical_screen_descriptor.packed_fields & 0x07) + 1);

    let (input, global_color_table) = if has_global_color_table {
        let (input, color_table) = parse_color_table(input, global_color_table_size)?;
        (input, Some(color_table))
    } else {
        (input, None)
    };

    let (input, blocks) = many0(
        alt((
            parse_graphic_control_extension.map(Block::GraphicControlExtension),
            parse_image_descriptor.map(Block::ImageDescriptor),
            tag([0x21]).and(tag([0x01])).map(|_| Block::PlainTextExtension),
            tag([0x21]).and(tag([0xFF])).map(|_| Block::ApplicationExtension),
            tag([0x21]).and(tag([0xFE])).map(|_| Block::CommentExtension),
        ))
    )(input)?;

    Ok((input, Gif {
        header,
        logical_screen_descriptor,
        global_color_table,
        blocks,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    
    if args.len() != 2 {
        eprintln!("Usage: {} <input_gif_file>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];
    let input = match fs::read(filename) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            process::exit(1);
        }
    };

    match parse_gif(&input) {
        Ok((_, gif)) => println!("Successfully parsed GIF: {:?}", gif),
        Err(e) => {
            eprintln!("Failed to parse GIF: {:?}", e);
            process::exit(1);
        }
    }
}