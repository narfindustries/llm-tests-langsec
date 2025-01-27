use nom::{
    bytes::complete::{tag, take, take_while},
    multi::{count, many0},
    number::complete::{le_u8, le_u16, le_u32},
    sequence::{tuple, preceded},
    IResult,
    combinator::{map, opt},
};
use std::fs::File;
use std::io::Read;
use std::env;

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
struct GraphicControlExtension {
    block_size: u8,
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
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
struct GifFile {
    header: GifHeader,
    screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    blocks: Vec<GifBlock>,
}

#[derive(Debug)]
enum GifBlock {
    ImageBlock {
        descriptor: ImageDescriptor,
        local_color_table: Option<Vec<ColorTableEntry>>,
        image_data: Vec<u8>,
    },
    GraphicControlExtension(GraphicControlExtension),
    ApplicationExtension(Vec<u8>),
    CommentExtension(Vec<u8>),
    PlainTextExtension(Vec<u8>),
    Trailer,
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

fn parse_color_table(input: &[u8], table_size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    count(
        map(
            tuple((le_u8, le_u8, le_u8)),
            |(red, green, blue)| ColorTableEntry { red, green, blue }
        ),
        table_size
    )(input)
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    preceded(
        tag(&[0x21, 0xF9]),
        map(
            tuple((
                le_u8,
                le_u8,
                le_u16,
                le_u8,
                tag(&[0x00])
            )),
            |(block_size, packed_fields, delay_time, transparent_color_index, _)| 
            GraphicControlExtension {
                block_size,
                packed_fields,
                delay_time,
                transparent_color_index,
            }
        )
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
            le_u8
        )),
        |(separator, left, top, width, height, packed_fields)| 
        ImageDescriptor {
            separator: separator[0],
            left,
            top,
            width,
            height,
            packed_fields,
        }
    )(input)
}

fn parse_image_block(input: &[u8]) -> IResult<&[u8], GifBlock> {
    let (input, descriptor) = parse_image_descriptor(input)?;
    let has_local_color_table = descriptor.packed_fields & 0x80 != 0;
    let local_color_table_size = 1 << ((descriptor.packed_fields & 0x07) + 1);

    let (input, local_color_table) = if has_local_color_table {
        let (input, table) = parse_color_table(input, local_color_table_size)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, image_data) = take_while(|_| true)(input)?;

    Ok((input, GifBlock::ImageBlock {
        descriptor,
        local_color_table,
        image_data: image_data.to_vec(),
    }))
}

fn parse_gif_blocks(input: &[u8]) -> IResult<&[u8], Vec<GifBlock>> {
    let mut blocks = Vec::new();
    let mut remaining_input = input;

    while !remaining_input.is_empty() {
        match remaining_input[0] {
            0x21 => {
                match remaining_input[1] {
                    0xF9 => {
                        let (input, extension) = parse_graphic_control_extension(remaining_input)?;
                        blocks.push(GifBlock::GraphicControlExtension(extension));
                        remaining_input = input;
                    },
                    0xFF => {
                        // Application Extension
                        let (input, _) = take_while(|_| true)(remaining_input)?;
                        blocks.push(GifBlock::ApplicationExtension(Vec::new()));
                        remaining_input = input;
                    },
                    0xFE => {
                        // Comment Extension
                        let (input, _) = take_while(|_| true)(remaining_input)?;
                        blocks.push(GifBlock::CommentExtension(Vec::new()));
                        remaining_input = input;
                    },
                    0x01 => {
                        // Plain Text Extension
                        let (input, _) = take_while(|_| true)(remaining_input)?;
                        blocks.push(GifBlock::PlainTextExtension(Vec::new()));
                        remaining_input = input;
                    },
                    _ => break,
                }
            },
            0x2C => {
                let (input, block) = parse_image_block(remaining_input)?;
                blocks.push(block);
                remaining_input = input;
            },
            0x3B => {
                blocks.push(GifBlock::Trailer);
                break;
            },
            _ => break,
        }
    }

    Ok((remaining_input, blocks))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GifFile> {
    let (input, header) = parse_gif_header(input)?;
    let (input, screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let has_global_color_table = screen_descriptor.packed_fields & 0x80 != 0;
    let global_color_table_size = 1 << ((screen_descriptor.packed_fields & 0x07) + 1);

    let (input, global_color_table) = if has_global_color_table {
        let (input, table) = parse_color_table(input, global_color_table_size)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, blocks) = parse_gif_blocks(input)?;

    Ok((input, GifFile {
        header,
        screen_descriptor,
        global_color_table,
        blocks,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <gif_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Could not open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Could not read file");

    match parse_gif(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Error parsing GIF: {:?}", e),
    }
}