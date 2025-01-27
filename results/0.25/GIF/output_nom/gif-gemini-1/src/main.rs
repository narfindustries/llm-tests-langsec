use std::env;
use std::fs;
use std::error::Error;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    multi::count,
    number::complete::{le_u16, le_u32},
    sequence::{preceded, tuple},
    IResult,
};

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 6],
    version: [u8; 3],
}

#[derive(Debug)]
struct GifScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct GifImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
    local_color_table_flag: bool,
    lct_size: Option<u8>,
    lct: Option<Vec<u8>>,
}


#[derive(Debug)]
struct GifGraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
    terminator: u8,
}

#[derive(Debug)]
struct GifCommentExtension {
    comment: Vec<u8>,
}

#[derive(Debug)]
struct GifApplicationExtension {
    application_identifier: [u8; 8],
    authentication_code: [u8; 3],
    data: Vec<u8>,
}

#[derive(Debug)]
enum GifExtension {
    GraphicControl(GifGraphicControlExtension),
    Comment(GifCommentExtension),
    Application(GifApplicationExtension),
}


fn gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = tag("GIF87a")(input)?;
    let (input, version) = take(3usize)(input)?;
    Ok((input, GifHeader { signature: *signature, version: *version }))
}

fn gif_screen_descriptor(input: &[u8]) -> IResult<&[u8], GifScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) = tuple((le_u16, le_u16, take(1usize), take(1usize), take(1usize)))(input)?;
    Ok((input, GifScreenDescriptor { width, height, packed_fields: packed_fields[0], background_color_index: background_color_index[0], pixel_aspect_ratio: pixel_aspect_ratio[0] }))
}

fn gif_image_descriptor(input: &[u8]) -> IResult<&[u8], GifImageDescriptor> {
    let (input, (left, top, width, height, packed_fields)) = tuple((le_u16, le_u16, le_u16, le_u16, take(1usize)))(input)?;
    let local_color_table_flag = (packed_fields[0] >> 7) & 1 != 0;
    let lct_size = if local_color_table_flag { Some((packed_fields[0] & 0x07) * 3 + 1) } else { None };
    let (input, lct) = opt(map(count(take(1usize), lct_size.unwrap_or(0) as usize), |bytes| bytes.concat()))(input)?;

    Ok((input, GifImageDescriptor { left, top, width, height, packed_fields: packed_fields[0], local_color_table_flag, lct_size, lct }))
}

fn gif_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GifGraphicControlExtension> {
    let (input, _) = tag("\x21\xf9")(input)?;
    let (input, (packed_fields, delay_time, transparent_color_index, terminator)) = tuple((take(1usize), le_u16, take(1usize), take(1usize)))(input)?;
    Ok((input, GifGraphicControlExtension { packed_fields: packed_fields[0], delay_time, transparent_color_index: transparent_color_index[0], terminator: terminator[0] }))
}

fn gif_comment_extension(input: &[u8]) -> IResult<&[u8], GifCommentExtension> {
    let (input, _) = tag("\x21\xfe")(input)?;
    let (input, comment) = recognize(many0(take(255usize)))(input)?;
    Ok((input, GifCommentExtension { comment: comment.to_vec() }))
}

fn gif_application_extension(input: &[u8]) -> IResult<&[u8], GifApplicationExtension> {
    let (input, _) = tag("\x21\xff")(input)?;
    let (input, (application_identifier, authentication_code, data)) = tuple((take(8usize), take(3usize), many0(take(255usize))))(input)?;
    Ok((input, GifApplicationExtension { application_identifier: *application_identifier, authentication_code: *authentication_code, data: data.concat() }))
}

fn gif_extension(input: &[u8]) -> IResult<&[u8], GifExtension> {
    let (input, _) = tag("\x21")(input)?;
    let (input, extension_type) = take(1usize)(input)?;
    match extension_type[0] {
        0xf9 => map(gif_graphic_control_extension, GifExtension::GraphicControl)(input),
        0xfe => map(gif_comment_extension, GifExtension::Comment)(input),
        0xff => map(gif_application_extension, GifExtension::Application)(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}


fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let data = fs::read(filename)?;

    let result = gif_header(&data);
    println!("{:?}", result);

    Ok(())
}

use nom::multi::many0;
