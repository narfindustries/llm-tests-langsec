use nom::{
    bytes::complete::tag,
    number::complete::{be_u16, u8},
    multi::{many_till, count},
    sequence::{tuple, terminated},
    combinator::{map, opt},
    branch::alt,
    IResult,
};
use std::fs;
use std::env;

#[derive(Debug)]
struct GIFHeader {
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
struct GlobalColorTable {
    colors: Vec<(u8, u8, u8)>,
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
struct LocalColorTable {
    colors: Vec<(u8, u8, u8)>,
}

#[derive(Debug)]
struct ImageData {
    lzw_code_size: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct GraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct PlainTextExtension {
    grid_left: u16,
    grid_top: u16,
    grid_width: u16,
    grid_height: u16,
    cell_width: u8,
    cell_height: u8,
    fg_color_index: u8,
    bg_color_index: u8,
    text_data: Vec<u8>,
}

#[derive(Debug)]
struct ApplicationExtension {
    identifier: [u8; 8],
    auth_code: [u8; 3],
    app_data: Vec<u8>,
}

#[derive(Debug)]
struct CommentExtension {
    comment_data: Vec<u8>,
}

#[derive(Debug)]
struct GIF {
    header: GIFHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<GlobalColorTable>,
    frames: Vec<Frame>,
    trailer: u8,
}

#[derive(Debug)]
enum Frame {
    Image {
        descriptor: ImageDescriptor,
        local_color_table: Option<LocalColorTable>,
        image_data: ImageData,
        graphic_control_extension: Option<GraphicControlExtension>,
    },
    PlainText(PlainTextExtension),
    Application(ApplicationExtension),
    Comment(CommentExtension),
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GIF> {
    let (input, header) = parse_header(input)?;
    let (input, lsd) = parse_logical_screen_descriptor(input)?;
    let (input, gct) = if lsd.packed_fields & 0x80 != 0 {
        let (input, table) = parse_global_color_table(1 << ((lsd.packed_fields & 0x07) + 1))(input)?;
        (input, Some(table))
    } else {
        (input, None)
    };
    let (input, (frames, _)) = many_till(parse_frame, tag(&[0x3B]))(input)?;
    Ok((input, GIF {
        header,
        logical_screen_descriptor: lsd,
        global_color_table: gct,
        frames,
        trailer: 0x3B,
    }))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GIFHeader> {
    let (input, signature) = map(tag("GIF"), |v: &[u8]| String::from_utf8(v.to_vec()).unwrap())(input)?;
    let (input, version) = map(tag("89a"), |v: &[u8]| String::from_utf8(v.to_vec()).unwrap())(input)?;
    Ok((input, GIFHeader { signature, version }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) =
        tuple((be_u16, be_u16, u8, u8, u8))(input)?;
    Ok((input, LogicalScreenDescriptor { width, height, packed_fields, background_color_index, pixel_aspect_ratio }))
}

fn parse_global_color_table(size: usize) -> impl Fn(&[u8]) -> IResult<&[u8], GlobalColorTable> {
    move |input| {
        let (input, colors) = map(
            count(tuple((u8, u8, u8)), size),
            |v| v.into_iter().map(|(r, g, b)| (r, g, b)).collect()
        )(input)?;
        Ok((input, GlobalColorTable { colors }))
    }
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, separator) = u8(input)?;
    let (input, (left, top, width, height, packed_fields)) =
        tuple((be_u16, be_u16, be_u16, be_u16, u8))(input)?;
    Ok((input, ImageDescriptor { separator, left, top, width, height, packed_fields }))
}

fn parse_local_color_table(size: usize) -> impl Fn(&[u8]) -> IResult<&[u8], LocalColorTable> {
    move |input| {
        let (input, colors) = map(
            count(tuple((u8, u8, u8)), size),
            |v| v.into_iter().map(|(r, g, b)| (r, g, b)).collect()
        )(input)?;
        Ok((input, LocalColorTable { colors }))
    }
}

fn parse_image_data(input: &[u8])