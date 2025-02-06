use nom::{
    bytes::complete::{tag, take},
    multi::{count, many0},
    number::complete::{le_u8, le_u16},
    sequence::{tuple, preceded},
    IResult,
    branch::alt,
    combinator::{map, cond},
    bytes::streaming::take_while1,
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
struct ImageDescriptor {
    left_position: u16,
    top_position: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct GraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
enum Extension {
    GraphicControl(GraphicControlExtension),
    PlainText,
    Application,
    Comment,
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    images: Vec<(ImageDescriptor, Option<Vec<ColorTableEntry>>, Vec<u8>)>,
    extensions: Vec<Extension>,
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

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    let (input, color_entries) = count(
        map(tuple((le_u8, le_u8, le_u8)), |(r, g, b)| ColorTableEntry { red: r, green: g, blue: b }),
        size
    )(input)?;

    Ok((input, color_entries))
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

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag([0x21, 0xF9, 0x04])(input)?;
    let (input, (packed_fields, delay_time, transparent_color_index, _)) = tuple((
        le_u8,
        le_u16,
        le_u8,
        tag([0x00])
    ))(input)?;

    Ok((input, GraphicControlExtension {
        packed_fields,
        delay_time,
        transparent_color_index,
    }))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    alt((
        map(parse_graphic_control_extension, Extension::GraphicControl),
        map(tag([0x21, 0x01]), |_| Extension::PlainText),
        map(tag([0x21, 0xFF]), |_| Extension::Application),
        map(tag([0x21, 0xFE]), |_| Extension::Comment)
    ))(input)
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, lzw_min_code_size) = le_u8(input)?;
    let (input, data_blocks) = many0(
        preceded(le_u8, take_while1(|_| true))
    )(input)?;

    Ok((input, data_blocks.concat()))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let global_color_table_size = (logical_screen_descriptor.packed_fields & 0x07) + 1;
    let (input, global_color_table) = cond(
        logical_screen_descriptor.packed_fields & 0x80 != 0,
        |i| parse_color_table(i, 1 << global_color_table_size)
    )(input)?;

    let (input, extensions) = many0(parse_extension)(input)?;

    let (input, images) = many0(|i| {
        let (i, image_descriptor) = parse_image_descriptor(i)?;
        let local_color_table_size = (image_descriptor.packed_fields & 0x07) + 1;
        let (i, local_color_table) = cond(
            image_descriptor.packed_fields & 0x80 != 0,
            |j| parse_color_table(j, 1 << local_color_table_size)
        )(i)?;
        let (i, image_data) = parse_image_data(i)?;
        Ok((i, (image_descriptor, local_color_table, image_data)))
    })(input)?;

    let (input, _) = tag([0x3B])(input)?;

    Ok((input, Gif {
        header,
        logical_screen_descriptor,
        global_color_table,
        images,
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
            println!("Parsed GIF: {:?}", gif);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse GIF: {:?}", e);
            std::process::exit(1);
        }
    }
}