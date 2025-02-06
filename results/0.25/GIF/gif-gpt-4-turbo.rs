use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct Color {
    r: u8,
    g: u8,
    b: u8,
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: u8,
    bg_color_index: u8,
    pixel_aspect_ratio: u8,
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
enum Block {
    ImageBlock(ImageDescriptor, Vec<u8>), // Simplified for example
    ExtensionBlock(u8, Vec<Vec<u8>>),
    Trailer,
}

#[derive(Debug)]
struct Gif {
    header: String,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<Color>>,
    blocks: Vec<Block>,
}

fn parse_color(input: &[u8]) -> IResult<&[u8], Color> {
    let (input, (r, g, b)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, Color { r, g, b }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, bg_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
            bg_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_global_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<Color>> {
    count(parse_color, size)(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (_, left, top, width, height, packed_fields)) =
        tuple((tag([0x2C]), le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
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

fn parse_extension_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, (label, block_size)) = preceded(tag([0x21]), tuple((le_u8, le_u8)))(input)?;
    let (input, data_blocks) = count(preceded(le_u8, take(block_size as usize)), block_size as usize)(input)?;
    let data_blocks_converted = data_blocks.into_iter().map(|d| d.to_vec()).collect();
    Ok((input, Block::ExtensionBlock(label, data_blocks_converted)))
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, block) = opt(preceded(
        tag([0x21]),
        map(tuple((le_u8, le_u8, take(1usize))), |(label, size, data)| {
            Block::ExtensionBlock(label, vec![data.to_vec()])
        }),
    ))(input)?;

    match block {
        Some(b) => Ok((input, b)),
        None => {
            let (input, trailer) = tag([0x3B])(input)?;
            Ok((input, Block::Trailer))
        }
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, (header, lsd)) = tuple((map(take(6usize), |s: &[u8]| String::from_utf8_lossy(s).into_owned()), parse_logical_screen_descriptor))(input)?;
    let global_color_table_size = if (lsd.packed_fields & 0b10000000) != 0 {
        Some(2usize.pow(((lsd.packed_fields & 0b00000111) + 1) as u32))
    } else {
        None
    };
    let (input, gct) = match global_color_table_size {
        Some(size) => {
            let (input, table) = parse_global_color_table(input, size)?;
            (input, Some(table))
        },
        None => (input, None),
    };
    let (input, blocks) = many0(parse_block)(input)?;
    Ok((
        input,
        Gif {
            header,
            logical_screen_descriptor: lsd,
            global_color_table: gct,
            blocks,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return Ok(());
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