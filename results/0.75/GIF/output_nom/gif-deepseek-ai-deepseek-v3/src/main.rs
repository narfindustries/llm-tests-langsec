use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::{count, length_data},
    number::complete::{be_u16, be_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

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
struct ImageDescriptor {
    left: u16,
    top: u16,
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
struct Gif {
    header: GifHeader,
    screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<(u8, u8, u8)>>,
    image_descriptors: Vec<ImageDescriptor>,
    local_color_tables: Vec<Option<Vec<(u8, u8, u8)>>>,
    image_data: Vec<Vec<u8>>,
    graphic_control_extensions: Vec<GraphicControlExtension>,
    plain_text_extensions: Vec<PlainTextExtension>,
    application_extensions: Vec<ApplicationExtension>,
    comment_extensions: Vec<CommentExtension>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = map(tag("GIF"), |s| String::from_utf8(s.to_vec()).unwrap())(input)?;
    let (input, version) = map(take(3usize), |s: &[u8]| String::from_utf8(s.to_vec()).unwrap())(input)?;
    Ok((input, GifHeader { signature, version }))
}

fn parse_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, width) = be_u16(input)?;
    let (input, height) = be_u16(input)?;
    let (input, packed_fields) = be_u8(input)?;
    let (input, bg_color_index) = be_u8(input)?;
    let (input, pixel_aspect_ratio) = be_u8(input)?;
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

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<(u8, u8, u8)>> {
    let (input, table) = count(tuple((be_u8, be_u8, be_u8)), size)(input)?;
    Ok((input, table))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag(&[0x2C])(input)?;
    let (input, left) = be_u16(input)?;
    let (input, top) = be_u16(input)?;
    let (input, width) =极::be_u16(input)?;
    let (input, height) = be_u16(input)?;
    let (input, packed_fields) = be_u8(input)?;
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

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag(&[0x21, 0xF9, 0x04])(input)?;
    let (input, packed_fields) = be_u8(input)?;
    let (input, delay_time) = be_u16(input)?;
    let (input, transparent_color_index极::be_u8(input)?;
    let (input, _) = tag(&[0x00])(input)?;
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
    let (input, _) = tag(&[0x21, 0x01, 0x0C])(input)?;
    let (input, grid_left) = be_u16(input)?;
    let (input, grid_top) = be_u16(input)?;
    let (input, grid_width) = be_u16(input)?;
    let (input, grid_height) = be_u16(input)?;
    let (input, cell_width) = be_u8(input)?;
    let (input, cell_height) = be_u8(input)?;
    let (input, fg_color_index) = be_u8(input)?;
    let (input, bg_color_index) = be_u8(input)?;
    let (input, text_data) = length_data(be_u8)(input)?;
    Ok((
        input,
        PlainTextExtension {
            grid_left,
            grid_top,
            grid_width,
            grid_height,
            cell_width,
            cell_height,
            fg_color_index,
            bg_color_index,
            text_data: text_data.to_vec(),
        },
    ))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, _) = tag(&[0x21, 0xFF, 0x极B])(input)?;
    let (input, identifier) = take(8usize)(input)?;
    let (input, auth_code) = take(3usize)(input)?;
    let (input, app_data) = length_data(be_u8)(input)?;
    Ok((
        input,
        ApplicationExtension {
            identifier: [identifier[0], identifier[1], identifier[2], identifier[3], identifier[4], identifier[5], identifier[6], identifier[7]],
            auth_code: [auth_code[0], auth_code[1], auth_code[2]],
            app_data: app_data.to_vec(),
        },
    ))
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], CommentExtension> {
    let (input, _) = tag(&[0x21, 0xFE])(input)?;
    let (input, comment_data) = length_data(be_u8)(input)?;
    Ok((
        input,
        CommentExtension {
            comment_data: comment_data.to_vec(),
        },
    ))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;
    let (input, screen_descriptor) = parse_screen_descriptor(input)?;
    let (input, global_color_table) = if (screen_descriptor.packed_fields & 0x80) != 0 {
        let size = 2usize.pow(((screen_descriptor.packed_fields & 0x07) + 1) as u32);
        let (input, table) = parse_color_table(input, size)?;
        (input, Some(table))
    } else {
        (input, None)
    };
    let mut image_descriptors = Vec::new();
    let mut local_color_tables = Vec::new();
    let mut image_data = Vec::new();
    let mut graphic_control_extensions = Vec::new();
    let mut plain_text_extensions = Vec::new();
    let mut application_extensions = Vec::new();
    let mut comment_extensions = Vec::new();
    let mut input = input;
    loop {
        if input.is_empty() {
            break;
        }
        match input[0] {
            0x2C => {
                let (new_input, image_descriptor) = parse_image_descriptor(input)?;
                image_descriptors.push(image_descriptor);
                let (new_input, local_color_table) = if (image_descriptors.last().unwrap().packed_fields & 0x80) != 0 {
                    let size = 2usize.pow(((image_descriptors.last().unwrap().packed_fields & 0x07) + 1) as u32);
                   