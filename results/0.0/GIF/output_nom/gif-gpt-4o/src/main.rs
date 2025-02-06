use nom::{
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::count,
    number::complete::{le_u16, le_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::Read;
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
struct Gif {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    image_descriptors: Vec<ImageDescriptor>,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, (signature, version)) = tuple((tag("GIF"), take(3usize)))(input)?;
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

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    count(
        map_res(take(3usize), |rgb: &[u8]| {
            Ok::<ColorTableEntry, nom::error::Error<&[u8]>>(ColorTableEntry {
                red: rgb[0],
                green: rgb[1],
                blue: rgb[2],
            })
        }),
        size,
    )(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag(&[0x2C])(input)?;
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

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_gif_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let global_color_table_flag = (logical_screen_descriptor.packed_fields & 0b1000_0000) >> 7;
    let global_color_table_size = 2usize.pow(((logical_screen_descriptor.packed_fields & 0b0000_0111) + 1) as u32);

    let (input, global_color_table) = if global_color_table_flag == 1 {
        let (input, table) = parse_color_table(input, global_color_table_size)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let mut input = input;
    let mut image_descriptors = Vec::new();

    while !input.is_empty() {
        if input[0] == 0x3B {
            break;
        }
        let (new_input, image_descriptor) = parse_image_descriptor(input)?;
        image_descriptors.push(image_descriptor);
        input = new_input;
    }

    Ok((
        input,
        Gif {
            header,
            logical_screen_descriptor,
            global_color_table,
            image_descriptors,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let filename = &args[1];
    let mut file = File::open(filename).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_gif(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }
}