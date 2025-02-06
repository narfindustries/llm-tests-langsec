use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{count, length_data, many0},
    number::complete::{le_u16, le_u8},
    sequence::{tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::Path;
use std::env;

#[derive(Debug)]
struct Gif {
    header: Header,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<Color>>,
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
    global_color_table_flag: bool,
    color_resolution: u8,
    sort_flag: bool,
    global_color_table_size: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct Color {
    r: u8,
    g: u8,
    b: u8,
}

#[derive(Debug)]
enum Block {
    ImageDescriptor(ImageDescriptor),
    Extension(Extension),
    Trailer,
}

#[derive(Debug)]
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    local_color_table_flag: bool,
    interlace_flag: bool,
    sort_flag: bool,
    local_color_table_size: u8,
    local_color_table: Option<Vec<Color>>,
    image_data: Vec<u8>,
}

#[derive(Debug)]
enum Extension {
    GraphicControlExtension(GraphicControlExtension),
    CommentExtension(Vec<u8>),
    PlainTextExtension(PlainTextExtension),
    ApplicationExtension(ApplicationExtension),
}

#[derive(Debug)]
struct GraphicControlExtension {
    disposal_method: u8,
    user_input_flag: bool,
    transparent_color_flag: bool,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct PlainTextExtension {
    text_grid_left: u16,
    text_grid_top: u16,
    text_grid_width: u16,
    text_grid_height: u16,
    char_cell_width: u8,
    char_cell_height: u8,
    text_fg_color_index: u8,
    text_bg_color_index: u8,
    plain_text_data: Vec<u8>,
}

#[derive(Debug)]
struct ApplicationExtension {
    application_identifier: String,
    application_authentication_code: String,
    application_data: Vec<u8>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, (signature, version)) = tuple((tag("GIF"), take(3usize)))(input)?;
    Ok((
        input,
        Header {
            signature: String::from_utf8_lossy(signature).into_owned(),
            version: String::from_utf8_lossy(version).into_owned(),
        },
    ))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed, background_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
    let global_color_table_flag = packed & 0b1000_0000 != 0;
    let color_resolution = (packed & 0b0111_0000) >> 4;
    let sort_flag = packed & 0b0000_1000 != 0;
    let global_color_table_size = packed & 0b0000_0111;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            global_color_table_flag,
            color_resolution,
            sort_flag,
            global_color_table_size,
            background_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_color(input: &[u8]) -> IResult<&[u8], Color> {
    let (input, (r, g, b)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, Color { r, g, b }))
}

fn parse_global_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<Color>> {
    count(parse_color, size)(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (_, left, top, width, height, packed)) =
        tuple((tag(&[0x2C]), le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
    let local_color_table_flag = packed & 0b1000_0000 != 0;
    let interlace_flag = packed & 0b0100_0000 != 0;
    let sort_flag = packed & 0b0010_0000 != 0;
    let local_color_table_size = packed & 0b0000_0111;
    let (input, local_color_table) = if local_color_table_flag {
        let size = 2usize.pow(local_color_table_size as u32 + 1);
        let (input, table) = parse_global_color_table(input, size)?;
        (input, Some(table))
    } else {
        (input, None)
    };
    let (input, image_data) = parse_image_data(input)?;
    Ok((
        input,
        ImageDescriptor {
            left,
            top,
            width,
            height,
            local_color_table_flag,
            interlace_flag,
            sort_flag,
            local_color_table_size,
            local_color_table,
            image_data,
        },
    ))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _lzw_minimum_code_size) = le_u8(input)?;
    let (input, blocks) = many0(length_data(le_u8))(input)?;
    let image_data: Vec<u8> = blocks.into_iter().flatten().cloned().collect();
    Ok((input, image_data))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, _) = tag(&[0x21])(input)?;
    let (input, label) = le_u8(input)?;
    match label {
        0xF9 => parse_graphic_control_extension(input),
        0xFE => parse_comment_extension(input),
        0x01 => parse_plain_text_extension(input),
        0xFF => parse_application_extension(input),
        _ => Err(nom::Err::Failure(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, (_, packed, delay_time, transparent_color_index, _)) =
        tuple((tag(&[0x04]), le_u8, le_u16, le_u8, tag(&[0x00])))(input)?;
    let disposal_method = (packed & 0b0001_1100) >> 2;
    let user_input_flag = packed & 0b0000_0010 != 0;
    let transparent_color_flag = packed & 0b0000_0001 != 0;
    Ok((
        input,
        Extension::GraphicControlExtension(GraphicControlExtension {
            disposal_method,
            user_input_flag,
            transparent_color_flag,
            delay_time,
            transparent_color_index,
        }),
    ))
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, comment_data) = parse_sub_blocks(input)?;
    Ok((input, Extension::CommentExtension(comment_data)))
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, (_, text_grid_left, text_grid_top, text_grid_width, text_grid_height, char_cell_width, char_cell_height, text_fg_color_index, text_bg_color_index)) =
        tuple((tag(&[0x0C]), le_u16, le_u16, le_u16, le_u16, le_u8, le_u8, le_u8, le_u8))(input)?;
    let (input, plain_text_data) = parse_sub_blocks(input)?;
    Ok((
        input,
        Extension::PlainTextExtension(PlainTextExtension {
            text_grid_left,
            text_grid_top,
            text_grid_width,
            text_grid_height,
            char_cell_width,
            char_cell_height,
            text_fg_color_index,
            text_bg_color_index,
            plain_text_data,
        }),
    ))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, (_, application_identifier, application_authentication_code)) =
        tuple((tag(&[0x0B]), take(8usize), take(3usize)))(input)?;
    let (input, application_data) = parse_sub_blocks(input)?;
    Ok((
        input,
        Extension::ApplicationExtension(ApplicationExtension {
            application_identifier: String::from_utf8_lossy(application_identifier).into_owned(),
            application_authentication_code: String::from_utf8_lossy(application_authentication_code).into_owned(),
            application_data,
        }),
    ))
}

fn parse_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, blocks) = many0(length_data(le_u8))(input)?;
    let data: Vec<u8> = blocks.into_iter().flatten().cloned().collect();
    Ok((input, data))
}

fn parse_trailer(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, _) = tag(&[0x3B])(input)?;
    Ok((input, Block::Trailer))
}

fn parse_blocks(input: &[u8]) -> IResult<&[u8], Vec<Block>> {
    many0(map(
        nom::branch::alt((
            map(parse_image_descriptor, Block::ImageDescriptor),
            map(parse_extension, Block::Extension),
            parse_trailer,
        )),
        |block| block,
    ))(input)
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let (input, global_color_table) = if logical_screen_descriptor.global_color_table_flag {
        let size = 2usize.pow(logical_screen_descriptor.global_color_table_size as u32 + 1);
        let (input, table) = parse_global_color_table(input, size)?;
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

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let path = Path::new(&args[1]);
    let mut file = File::open(&path)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }

    Ok(())
}