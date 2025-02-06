use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u8},
    sequence::tuple,
    IResult,
};
use std::fs;

#[derive(Debug)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
}

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
    bg_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct GraphicsControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
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
    Extension(Vec<u8>),
    ImageData(Vec<u8>),
}

#[derive(Debug)]
struct GifData {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<Color>>,
    blocks: Vec<Block>,
}

fn parse_color(input: &[u8]) -> IResult<&[u8], Color> {
    let (input, (red, green, blue)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, Color { red, green, blue }))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, (sig, ver)) = tuple((tag("GIF"), take(3usize)))(input)?;
    let signature = String::from_utf8(sig.to_vec()).unwrap();
    let version = String::from_utf8(ver.to_vec()).unwrap();
    Ok((input, GifHeader { signature, version }))
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

fn parse_global_color_table(input: &[u8], num_colors: usize) -> IResult<&[u8], Vec<Color>> {
    let mut colors = Vec::new();
    let mut remaining = input;
    for _ in 0..num_colors {
        let (new_remaining, color) = parse_color(remaining)?;
        remaining = new_remaining;
        colors.push(color);
    }
    Ok((remaining, colors))
}

fn parse_blocks(input: &[u8]) -> IResult<&[u8], Vec<Block>> {
    let mut blocks = Vec::new();
    let mut remaining = input;
    while remaining.len() > 0 && remaining[0] != 0x3B {
        let (new_remaining, data) = take(1usize)(remaining)?; // Simplified for now
        remaining = new_remaining;
        blocks.push(Block::Extension(data.to_vec()));
    }
    Ok((remaining, blocks))
}

fn parse_gif_data(input: &[u8]) -> IResult<&[u8], GifData> {
    let (input, header) = parse_header(input)?;
    let (input, lsd) = parse_logical_screen_descriptor(input)?;
    let gct_flag = (lsd.packed_fields & 0b10000000) >> 7;
    let gct_size = 1 << ((lsd.packed_fields & 0b00000111) + 1);

    let (input, global_color_table) = if gct_flag == 1 {
        let (input, table) = parse_global_color_table(input, gct_size)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, blocks) = parse_blocks(input)?;

    Ok((
        input,
        GifData {
            header,
            logical_screen_descriptor: lsd,
            global_color_table,
            blocks,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        println!("Usage: {} <filename>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = fs::read(filename).expect("Unable to read file");

    match parse_gif_data(&data) {
        Ok((_remaining, gif_data)) => {
            println!("Parsed GIF data: {:?}", gif_data);
        }
        Err(e) => {
            println!("Error parsing GIF: {:?}", e);
        }
    }
}