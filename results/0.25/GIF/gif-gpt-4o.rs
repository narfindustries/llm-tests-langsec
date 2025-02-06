use nom::bytes::complete::{tag, take};
use nom::number::complete::{le_u16, le_u8};
use nom::IResult;
use std::fs::File;
use std::io::Read;
use std::path::Path;
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
struct ImageDescriptor {
    image_separator: u8,
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    image_descriptors: Vec<ImageDescriptor>,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = tag("GIF")(input)?;
    let (input, version) = take(3usize)(input)?;
    Ok((input, GifHeader {
        signature: [signature[0], signature[1], signature[2]],
        version: [version[0], version[1], version[2]],
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

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    let mut entries = Vec::new();
    let mut input = input;
    for _ in 0..size {
        let (i, red) = le_u8(input)?;
        let (i, green) = le_u8(i)?;
        let (i, blue) = le_u8(i)?;
        entries.push(ColorTableEntry { red, green, blue });
        input = i;
    }
    Ok((input, entries))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, image_separator) = le_u8(input)?;
    let (input, left) = le_u16(input)?;
    let (input, top) = le_u16(input)?;
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
    Ok((input, ImageDescriptor {
        image_separator,
        left,
        top,
        width,
        height,
        packed_fields,
    }))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_gif_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    
    let global_color_table_flag = (logical_screen_descriptor.packed_fields & 0b1000_0000) >> 7;
    let size_of_global_color_table = 2usize.pow(((logical_screen_descriptor.packed_fields & 0b0000_0111) + 1) as u32);

    let (input, global_color_table) = if global_color_table_flag == 1 {
        let (input, table) = parse_color_table(input, size_of_global_color_table)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let mut input = input;
    let mut image_descriptors = Vec::new();

    while let Ok((i, image_descriptor)) = parse_image_descriptor(input) {
        image_descriptors.push(image_descriptor);
        input = i;
    }

    Ok((input, Gif {
        header,
        logical_screen_descriptor,
        global_color_table,
        image_descriptors,
    }))
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
        Ok((_remaining, gif)) => {
            println!("{:#?}", gif);
        }
        Err(e) => {
            eprintln!("Failed to parse GIF: {:?}", e);
        }
    }
}