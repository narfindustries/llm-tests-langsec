use nom::branch::alt;
use nom::bytes::complete::{tag, take};
use nom::character::complete::space0;
use nom::combinator::{map, opt};
use nom::multi::{many0, many_till};
use nom::number::complete::be_u16;
use nom::sequence::{tuple};
use nom::{IResult, Finish};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum DisposalMethod {
    NoDisposal = 0,
    DoNotDispose = 1,
    RestoreToBackground = 2,
    RestoreToPrevious = 3,
    NoOperation = 4,
    RestoreToBackgroundColor = 5,
    RestoreToPreviousBackgroundColor = 6,
    RestoreToPreviousAndKeepBackground = 7,
}

#[derive(Debug, PartialEq)]
struct GifHeader {
    signature: Vec<u8>,
    version: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug, PartialEq)]
struct GlobalColorTable {
    color_table: Vec<(u8, u8, u8)>,
}

#[derive(Debug, PartialEq)]
struct ImageDescriptor {
    image_separator: u8,
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug, PartialEq)]
struct LocalColorTable {
    color_table: Vec<(u8, u8, u8)>,
}

#[derive(Debug, PartialEq)]
struct GraphicControlExtension {
    extension_introducer: u8,
    extension_label: u8,
    block_size: u8,
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug, PartialEq)]
struct ApplicationExtension {
    extension_introducer: u8,
    extension_label: u8,
    block_size: u8,
    application_identifier: Vec<u8>,
    application_authentication_code: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct CommentExtension {
    extension_introducer: u8,
    extension_label: u8,
    comment_data: Vec<u8>,
}

#[derive(Debug, PartialEq)]
struct PlainTextExtension {
    extension_introducer: u8,
    extension_label: u8,
    block_size: u8,
    text_grid_left: u8,
    text_grid_top: u8,
    text_grid_width: u8,
    text_grid_height: u8,
    character_cell_width: u8,
    character_cell_height: u8,
    text_foreground_color_index: u8,
    text_background_color_index: u8,
    plain_text_data: Vec<u8>,
}

fn parse_signature<'a>(input: &'a [u8]) -> IResult<&'a [u8], Vec<u8>> {
    let (input, signature) = take(3usize)(input)?;
    Ok((input, signature.to_vec()))
}

fn parse_version<'a>(input: &'a [u8]) -> IResult<&'a [u8], Vec<u8>> {
    let (input, version) = take(3usize)(input)?;
    Ok((input, version.to_vec()))
}

fn parse_packed_fields<'a>(input: &'a [u8]) -> IResult<&'a [u8], u8> {
    let (input, packed_fields) = take(1usize)(input)?;
    Ok((input, packed_fields[0]))
}

fn parse_logical_screen_descriptor<'a>(input: &'a [u8]) -> IResult<&'a [u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) =
        tuple((be_u16, be_u16, parse_packed_fields, take(1usize), take(1usize)))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
            background_color_index: background_color_index[0],
            pixel_aspect_ratio: pixel_aspect_ratio[0],
        },
    ))
}

fn parse_global_color_table<'a>(
    input: &'a [u8],
    size: u8,
    logical_screen_descriptor: &'a LogicalScreenDescriptor,
) -> IResult<&'a [u8], GlobalColorTable> {
    let num_colors = 2u8.pow((logical_screen_descriptor.packed_fields & 0x07) as u32 + 1) as usize;
    let (input, color_table) = many_till(
        tuple((take(1usize), take(1usize), take(1usize))),
        take(1usize),
    )(input)?;
    let color_table: Vec<(u8, u8, u8)> = color_table
        .into_iter()
        .map(|(r, g, b)| (r[0], g[0], b[0]))
        .collect();
    Ok((input, GlobalColorTable { color_table }))
}

fn parse_image_descriptor<'a>(input: &'a [u8]) -> IResult<&'a [u8], ImageDescriptor> {
    let (input, (image_separator, left, top, width, height, packed_fields)) =
        tuple((take(1usize), be_u16, be_u16, be_u16, be_u16, parse_packed_fields))(input)?;
    Ok((
        input,
        ImageDescriptor {
            image_separator: image_separator[0],
            left,
            top,
            width,
            height,
            packed_fields,
        },
    ))
}

fn parse_local_color_table<'a>(input: &'a [u8], image_descriptor: &'a ImageDescriptor) -> IResult<&'a [u8], LocalColorTable> {
    let num_colors = 2u8.pow((image_descriptor.packed_fields & 0x07) as u32 + 1) as usize;
    let (input, color_table) = many_till(
        tuple((take(1usize), take(1usize), take(1usize))),
        take(1usize),
    )(input)?;
    let color_table: Vec<(u8, u8, u8)> = color_table
        .into_iter()
        .map(|(r, g, b)| (r[0], g[0], b[0]))
        .collect();
    Ok((input, LocalColorTable { color_table }))
}

fn parse_graphic_control_extension<'a>(input: &'a [u8]) -> IResult<&'a [u8], GraphicControlExtension> {
    let (input, (extension_introducer, extension_label, block_size, packed_fields, delay_time, transparent_color_index)) =
        tuple((take(1usize), take(1usize), take(1usize), parse_packed_fields, be_u16, take(1usize)))(input)?;
    Ok((
        input,
        GraphicControlExtension {
            extension_introducer: extension_introducer[0],
            extension_label: extension_label[0],
            block_size: block_size[0],
            packed_fields,
            delay_time,
            transparent_color_index: transparent_color_index[0],
        },
    ))
}

fn parse_application_extension<'a>(input: &'a [u8]) -> IResult<&'a [u8], ApplicationExtension> {
    let (input, (extension_introducer, extension_label, block_size, application_identifier, application_authentication_code)) =
        tuple((take(1usize), take(1usize), take(1usize), take(11usize), take(3usize)))(input)?;
    Ok((
        input,
        ApplicationExtension {
            extension_introducer: extension_introducer[0],
            extension_label: extension_label[0],
            block_size: block_size[0],
            application_identifier: application_identifier.to_vec(),
            application_authentication_code: application_authentication_code.to_vec(),
        },
    ))
}

fn parse_comment_extension<'a>(input: &'a [u8]) -> IResult<&'a [u8], CommentExtension> {
    let (input, (extension_introducer, extension_label, comment_data)) =
        tuple((take(1usize), take(1usize), many0(take(1usize))))(input)?;
    let comment_data: Vec<u8> = comment_data.into_iter().flatten().cloned().collect();
    Ok((
        input,
        CommentExtension {
            extension_introducer: extension_introducer[0],
            extension_label: extension_label[0],
            comment_data,
        },
    ))
}

fn parse_plain_text_extension<'a>(input: &'a [u8]) -> IResult<&'a [u8], PlainTextExtension> {
    let (input, (extension_introducer, extension_label, block_size, text_grid_left, text_grid_top, text_grid_width, text_grid_height, character_cell_width, character_cell_height, text_foreground_color_index, text_background_color_index, plain_text_data)) =
        tuple((take(1usize), take(1usize), take(1usize), take(1usize), take(1usize), take(1usize), take(1usize), take(1usize), take(1usize), take(1usize), take(1usize), many0(take(1usize))))(input)?;
    let plain_text_data: Vec<u8> = plain_text_data.into_iter().flatten().cloned().collect();
    Ok((
        input,
        PlainTextExtension {
            extension_introducer: extension_introducer[0],
            extension_label: extension_label[0],
            block_size: block_size[0],
            text_grid_left: text_grid_left[0],
            text_grid_top: text_grid_top[0],
            text_grid_width: text_grid_width[0],
            text_grid_height: text_grid_height[0],
            character_cell_width: character_cell_width[0],
            character_cell_height: character_cell_height[0],
            text_foreground_color_index: text_foreground_color_index[0],
            text_background_color_index: text_background_color_index[0],
            plain_text_data,
        },
    ))
}

fn parse_gif<'a>(input: &'a [u8]) -> IResult<&'a [u8], ()> {
    let (input, (signature, version, logical_screen_descriptor)) = tuple((
        parse_signature,
        parse_version,
        parse_logical_screen_descriptor,
    ))(input)?;
    let (input, global_color_table) = if (logical_screen_descriptor.packed_fields & 0x80) != 0 {
        parse_global_color_table(input, logical_screen_descriptor.packed_fields, &logical_screen_descriptor)
    } else {
        Ok((input, GlobalColorTable { color_table: Vec::new() }))
    }?;
    let (input, _) = many0(
        alt((
            map(parse_image_descriptor, |image_descriptor| {
                if (image_descriptor.packed_fields & 0x80) != 0 {
                    parse_local_color_table(input, &image_descriptor).unwrap().0
                } else {
                    input
                }
            }),
            map(parse_graphic_control_extension, |_| input),
            map(parse_application_extension, |_| input),
            map(parse_comment_extension, |_| input),
            map(parse_plain_text_extension, |_| input),
        ))
    )(input)?;
    Ok((input, ()))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");
    let result = parse_gif(&buffer).finish().unwrap();
    if result.0.len() != 1 || result.0[0] != 0x3b {
        println!("Invalid GIF file");
    }
}