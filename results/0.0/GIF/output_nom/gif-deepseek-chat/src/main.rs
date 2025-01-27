use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct GIFHeader {
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
struct ColorTable {
    colors: Vec<(u8, u8, u8)>,
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
struct GIF {
    header: GIFHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<ColorTable>,
    blocks: Vec<Block>,
}

#[derive(Debug)]
enum Block {
    Image(ImageDescriptor, Option<ColorTable>, Vec<u8>),
    Extension(u8, Vec<u8>),
    Trailer,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GIFHeader> {
    let (input, (signature, version)) = tuple((take(3usize), take(3usize)))(input)?;
    Ok((
        input,
        GIFHeader {
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

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], ColorTable> {
    let (input, colors) = take(size * 3)(input)?;
    let colors = colors
        .chunks(3)
        .map(|chunk| (chunk[0], chunk[1], chunk[2]))
        .collect();
    Ok((input, ColorTable { colors }))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (left, top, width, height, packed_fields)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
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

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, block_type) = le_u8(input)?;
    match block_type {
        0x2C => {
            let (input, descriptor) = parse_image_descriptor(input)?;
            let (input, local_color_table) = if descriptor.packed_fields & 0x80 != 0 {
                let size = 1 << ((descriptor.packed_fields & 0x07) + 1);
                let (input, table) = parse_color_table(input, size)?;
                (input, Some(table))
            } else {
                (input, None)
            };
            let (input, image_data) = parse_image_data(input)?;
            Ok((input, Block::Image(descriptor, local_color_table, image_data)))
        }
        0x21 => {
            let (input, extension_type) = le_u8(input)?;
            let (input, extension_data) = parse_extension_data(input)?;
            Ok((input, Block::Extension(extension_type, extension_data)))
        }
        0x3B => Ok((input, Block::Trailer)),
        _ => Err(nom::Err::Error((input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, lzw_minimum_code_size) = le_u8(input)?;
    let (input, sub_blocks) = parse_sub_blocks(input)?;
    Ok((input, sub_blocks))
}

fn parse_extension_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, sub_blocks) = parse_sub_blocks(input)?;
    Ok((input, sub_blocks))
}

fn parse_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut input = input;
    let mut sub_blocks = Vec::new();
    loop {
        let (new_input, block_size) = le_u8(input)?;
        if block_size == 0 {
            break;
        }
        let (new_input, block_data) = take(block_size)(new_input)?;
        sub_blocks.extend_from_slice(block_data);
        input = new_input;
    }
    Ok((input, sub_blocks))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GIF> {
    let (input, header) = parse_gif_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let (input, global_color_table) = if logical_screen_descriptor.packed_fields & 0x80 != 0 {
        let size = 1 << ((logical_screen_descriptor.packed_fields & 0x07) + 1);
        let (input, table) = parse_color_table(input, size)?;
        (input, Some(table))
    } else {
        (input, None)
    };
    let mut blocks = Vec::new();
    let mut input = input;
    loop {
        let (new_input, block) = parse_block(input)?;
        input = new_input;
        match block {
            Block::Trailer => break,
            _ => blocks.push(block),
        }
    }
    Ok((
        input,
        GIF {
            header,
            logical_screen_descriptor,
            global_color_table,
            blocks,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <gif file>", args[0]);
        return Ok(());
    }
    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    let result = parse_gif(&buffer);
    match result {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }
    Ok(())
}