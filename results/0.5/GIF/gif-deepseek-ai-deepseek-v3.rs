use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::{count, length_data},
    number::complete::{le_u16, le_u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs, io::Read};

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
struct PlainTextExtension {
    text_grid_left: u16,
    text_grid_top: u16,
    text_grid_width: u16,
    text_grid_height: u16,
    char_cell_width: u8,
    char_cell_height: u8,
    text_fg_color_index: u8,
    text_bg_color_index: u8,
}

#[derive(Debug)]
struct ApplicationExtension {
    identifier: String,
    auth_code: String,
}

#[derive(Debug)]
struct CommentExtension {
    comment: String,
}

#[derive(Debug)]
enum Block {
    GraphicControl(GraphicControlExtension),
    PlainText(PlainTextExtension),
    Application(ApplicationExtension),
    Comment(CommentExtension),
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    blocks: Vec<Block>,
    image_data: Vec<u8>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = map(tag("GIF"), |s| String::from_utf8_lossy(s).into_owned())(input)?;
    let (input, version) = map(tag("89a"), |s| String::from_utf8_lossy(s).into_owned())(input)?;
    Ok((input, GifHeader { signature, version }))
}

fn parse_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, bg_color_index) = le_u8(input)?;
    let (input, pixel_aspect_ratio) = le_u8(input)?;
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

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    let (input, entries) = count(
        map(tuple((le_u8, le_u8, le_u8)), |(red, green, blue)| {
            ColorTableEntry { red, green, blue }
        }),
        size,
    )(input)?;
    Ok((input, entries))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag([0x2C])(input)?;
    let (input, left_position) = le_u16(input)?;
    let (input, top_position) = le_u16(input)?;
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
    Ok((
        input,
        ImageDescriptor {
            left_position,
            top_position,
            width,
            height,
            packed_fields,
        },
    ))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag([0x21, 0xF9, 0x04])(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, delay_time) = le_u16(input)?;
    let (input, transparent_color_index) = le_u8(input)?;
    let (input, _) = tag([0x00])(input)?;
    Ok((
        input,
        GraphicControlExtension {
            packed_fields,
            delay_time,
            transparent_color_index,
        },
    ))
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], PlainTextExtension> {
    let (input, _) = tag([0x21, 0x01, 极客时间 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技 极客邦科技