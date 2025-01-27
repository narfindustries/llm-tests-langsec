use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{count, many0},
    number::complete::{le_u16, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct ColorTableEntry {
    red: u8,
    green: u8,
    blue: u8,
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
struct ImageDescriptor {
    left_position: u16,
    top_position: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
enum Block {
    ImageBlock(ImageDescriptor, Vec<u8>), // Simplified: actual GIF uses LZW compression
    ExtensionBlock(u8, Vec<u8>),
    UnknownBlock(u8),
}

#[derive(Debug)]
struct Gif {
    header: String,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    blocks: Vec<Block>,
}

fn parse_color_table_entry(input: &[u8]) -> IResult<&[u8], ColorTableEntry> {
    map(tuple((le_u8, le_u8, le_u8)), |(red, green, blue)| ColorTableEntry { red, green, blue })(input)
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
        tuple((le_u16, le_u16, le_u16, le_u16, le_u8)),
        |(left_position, top_position, width, height, packed_fields)| ImageDescriptor {
            left_position,
            top_position,
            width,
            height,
            packed_fields,
        },
    )(input)
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, block_label) = le_u8(input)?;
    match block_label {
        0x2C => {
            let (input, image_descriptor) = parse_image_descriptor(input)?;
            let (input, image_data) = preceded(tag(&[0x00]), take(image_descriptor.width as usize * image_descriptor.height as usize))(input)?;
            Ok((input, Block::ImageBlock(image_descriptor, image_data.to_vec())))
        }
        0x21 => {
            let (input, extension_data) = preceded(le_u8, many0(le_u8))(input)?;
            Ok((input, Block::ExtensionBlock(block_label, extension_data)))
        }
        _ => Ok((input, Block::UnknownBlock(block_label))),
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, _) = tag("GIF")(input)?;
    let (input, version) = map_res(take(3usize), std::str::from_utf8)(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let (input, global_color_table) = if logical_screen_descriptor.packed_fields & 0b1000_0000 > 0 {
        let size = 1 << ((logical_screen_descriptor.packed_fields & 0b0000_0111) + 1);
        map(count(parse_color_table_entry, size as usize), Some)(input)?
    } else {
        (input, None)
    };
    let (input, blocks) = many0(parse_block)(input)?;

    Ok((
        input,
        Gif {
            header: format!("GIF{}", version),
            logical_screen_descriptor,
            global_color_table,
            blocks,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "No file specified"));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((_, gif)) => {
            println!("{:#?}", gif);
        }
        Err(e) => {
            println!("Failed to parse GIF: {:?}", e);
        }
    }

    Ok(())
}