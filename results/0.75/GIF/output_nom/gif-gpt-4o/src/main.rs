use nom::bytes::complete::{tag, take};
use nom::number::complete::{le_u16, le_u8};
use nom::IResult;
use std::fs::File;
use std::io::Read;
use std::env;
use std::path::Path;

#[derive(Debug)]
struct Gif {
    header: Header,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<Rgb>>,
    blocks: Vec<Block>,
}

#[derive(Debug)]
struct Header {
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
struct Rgb {
    r: u8,
    g: u8,
    b: u8,
}

#[derive(Debug)]
enum Block {
    Image(ImageDescriptor, Option<Vec<Rgb>>, Vec<u8>),
    Extension(u8, Vec<u8>),
    Trailer,
}

#[derive(Debug)]
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, signature) = take(3usize)(input)?;
    let (input, version) = take(3usize)(input)?;
    Ok((
        input,
        Header {
            signature: String::from_utf8_lossy(signature).into_owned(),
            version: String::from_utf8_lossy(version).into_owned(),
        },
    ))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, background_color_index) = le_u8(input)?;
    let (input, pixel_aspect_ratio) = le_u8(input)?;
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

fn parse_rgb(input: &[u8]) -> IResult<&[u8], Rgb> {
    let (input, r) = le_u8(input)?;
    let (input, g) = le_u8(input)?;
    let (input, b) = le_u8(input)?;
    Ok((input, Rgb { r, g, b }))
}

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<Rgb>> {
    let mut table = Vec::with_capacity(size);
    let mut input = input;
    for _ in 0..size {
        let (i, rgb) = parse_rgb(input)?;
        table.push(rgb);
        input = i;
    }
    Ok((input, table))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag(b",")(input)?;
    let (input, left) = le_u16(input)?;
    let (input, top) = le_u16(input)?;
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
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

fn parse_image_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, lzw_min_code_size) = le_u8(input)?;
    let mut blocks = Vec::new();
    let mut input = input;
    loop {
        let (i, block_size) = le_u8(input)?;
        if block_size == 0 {
            break;
        }
        let (i, data) = take(block_size)(i)?;
        blocks.extend_from_slice(data);
        input = i;
    }
    Ok((input, blocks))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, _) = tag(b"!")(input)?;
    let (input, label) = le_u8(input)?;
    let mut blocks = Vec::new();
    let mut input = input;
    loop {
        let (i, block_size) = le_u8(input)?;
        if block_size == 0 {
            break;
        }
        let (i, data) = take(block_size)(i)?;
        blocks.extend_from_slice(data);
        input = i;
    }
    Ok((input, Block::Extension(label, blocks)))
}

fn parse_trailer(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, _) = tag(b";")(input)?;
    Ok((input, Block::Trailer))
}

fn parse_blocks(input: &[u8]) -> IResult<&[u8], Vec<Block>> {
    let mut blocks = Vec::new();
    let mut input = input;
    loop {
        if input.is_empty() {
            break;
        }
        match input[0] {
            b',' => {
                let (i, image_descriptor) = parse_image_descriptor(input)?;
                let (i, local_color_table) = if image_descriptor.packed_fields & 0x80 != 0 {
                    let size = 2_usize.pow((image_descriptor.packed_fields & 0x07) as u32 + 1);
                    let (i, table) = parse_color_table(i, size)?;
                    (i, Some(table))
                } else {
                    (i, None)
                };
                let (i, image_data) = parse_image_data(i)?;
                blocks.push(Block::Image(
                    image_descriptor,
                    local_color_table,
                    image_data,
                ));
                input = i;
            }
            b'!' => {
                let (i, extension) = parse_extension(input)?;
                blocks.push(extension);
                input = i;
            }
            b';' => {
                let (i, trailer) = parse_trailer(input)?;
                blocks.push(trailer);
                input = i;
                break;
            }
            _ => break,
        }
    }
    Ok((input, blocks))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let global_color_table_size = if logical_screen_descriptor.packed_fields & 0x80 != 0 {
        2_usize.pow((logical_screen_descriptor.packed_fields & 0x07) as u32 + 1)
    } else {
        0
    };
    let (input, global_color_table) = if global_color_table_size > 0 {
        let (input, table) = parse_color_table(input, global_color_table_size)?;
        (input, Some(table))
    } else {
        (input, None)
    };
    let (input, blocks) = parse_blocks(input)?;
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
        eprintln!("Usage: {} <filename>", args[0]);
        return;
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path).expect("Unable to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Unable to read file");

    match parse_gif(&buffer) {
        Ok((_, gif)) => {
            println!("{:#?}", gif);
        }
        Err(e) => {
            eprintln!("Failed to parse GIF: {:?}", e);
        }
    }
}