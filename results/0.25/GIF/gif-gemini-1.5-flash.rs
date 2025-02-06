use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, rest},
    error::ErrorKind,
    multi::count,
    number::complete::{le_u16, le_u8},
    sequence::{tuple, preceded},
    IResult,
};
use std::fs;
use std::path::Path;

#[derive(Debug)]
struct GifHeader {
    signature: String,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorEntry>>,
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: PackedFields,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct PackedFields {
    global_color_table_flag: bool,
    color_resolution: u8,
    sort_flag: bool,
    size_of_global_color_table: u8,
}

#[derive(Debug)]
struct ColorEntry {
    red: u8,
    green: u8,
    blue: u8,
}

#[derive(Debug)]
struct ImageDescriptor {
    image_separator: u8,
    image_left_position: u16,
    image_top_position: u16,
    image_width: u16,
    image_height: u16,
    packed_fields: PackedFields,
    local_color_table: Option<Vec<ColorEntry>>,
    image_data: Vec<u8>,
}


#[derive(Debug)]
enum Extension {
    GraphicControl(GraphicControlExtension),
    PlainText(PlainTextExtension),
    Comment(CommentExtension),
    Application(ApplicationExtension),
}

#[derive(Debug)]
struct GraphicControlExtension {
    block_size: u8,
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
    terminator: u8,
}

#[derive(Debug)]
struct PlainTextExtension {
    block_size: u8,
    text_grid_left: u16,
    text_grid_top: u16,
    text_grid_width: u16,
    text_grid_height: u16,
    cell_width: u8,
    cell_height: u8,
    text_data: Vec<u8>,
    terminator: u8,
}

#[derive(Debug)]
struct CommentExtension {
    comment_data: Vec<u8>,
}

#[derive(Debug)]
struct ApplicationExtension {
    application_identifier: String,
    application_authentication_code: String,
    application_data: Vec<u8>,
}


fn parse_packed_fields(input: &[u8]) -> IResult<&[u8], PackedFields> {
    let (input, packed_fields) = le_u8(input)?;
    Ok((
        input,
        PackedFields {
            global_color_table_flag: (packed_fields >> 7) & 1 == 1,
            color_resolution: (packed_fields >> 4) & 7,
            sort_flag: (packed_fields >> 3) & 1 == 1,
            size_of_global_color_table: packed_fields & 7,
        },
    ))
}

fn parse_color_entry(input: &[u8]) -> IResult<&[u8], ColorEntry> {
    let (input, (red, green, blue)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((
        input,
        ColorEntry {
            red,
            green,
            blue,
        },
    ))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag(b",")(input)?;
    let (input, (left, top, width, height, packed_fields)) = tuple((
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        parse_packed_fields,
    ))(input)?;
    let (input, local_color_table) = opt(preceded(
        take(1usize),
        count(parse_color_entry, 1 << (packed_fields.size_of_global_color_table + 1) as usize),
    ))(input)?;
    let (input, image_data) = rest(input)?;
    Ok((
        input,
        ImageDescriptor {
            image_separator: b',',
            image_left_position: left,
            image_top_position: top,
            image_width: width,
            image_height: height,
            packed_fields,
            local_color_table,
            image_data: image_data.to_vec(),
        },
    ))
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = map_res(take(6usize), std::str::from_utf8)(input)?;
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, parse_packed_fields, le_u8, le_u8))(input)?;
    let (input, global_color_table) = opt(preceded(
        take(1usize),
        count(parse_color_entry, 1 << (packed_fields.size_of_global_color_table + 1) as usize),
    ))(input)?;
    Ok((
        input,
        GifHeader {
            signature: signature.to_string(),
            logical_screen_descriptor: LogicalScreenDescriptor {
                width,
                height,
                packed_fields,
                background_color_index,
                pixel_aspect_ratio,
            },
            global_color_table,
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gif_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let data = fs::read(path).expect("Failed to read file");

    match parse_gif_header(&data) {
        Ok((_, header)) => println!("GIF Header: {:?}", header),
        Err(e) => println!("Error parsing GIF header: {:?}", e),
    }
}
