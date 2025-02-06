use nom::{
    bytes::complete::tag,
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
}

#[derive(Debug)]
struct ApplicationExtension {
    identifier: [u8; 8],
    auth_code: [u8; 3],
}

#[derive(Debug)]
struct CommentExtension {
    comment: Vec<u8>,
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
    image_descriptor: ImageDescriptor,
    local_color_table: Option<Vec<ColorTableEntry>>,
    image_data: Vec<u8>,
    blocks: Vec<Block>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = tag("GIF")(input)?;
    let (input, version) = tag("89a")(input)?;
    Ok((
        input,
        GifHeader {
            signature: signature.to_string(),
            version: version.to_string(),
        },
    ))
}

fn parse_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, bg_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
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
    let mut entries = Vec::new();
    let mut input = input;
    for _ in 0..size {
        let (new_input, (red, green, blue)) = tuple((le_u8, le_u8, le_u8))(input)?;
        entries.push(ColorTableEntry { red, green, blue });
        input = new_input;
    }
    Ok((input, entries))
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

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag(&[0x21, 0xF9, 0x04])(input)?;
    let (input, (packed_fields, delay_time, transparent_color_index, _)) =
        tuple((le_u8, le_u16, le_u8, tag(&[0x00])))(input)?;
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
    let (input, (grid_left, grid_top, grid_width, grid_height, cell_width, cell_height, fg_color_index, bg_color_index)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u8, le_u8, le_u8, le_u8))(input)?;
    let (input, _) = tag(&[0x00])(input)?;
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
        },
    ))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[极], ApplicationExtension> {
    let (input, _) = tag(&[0x21, 0xFF, 0x0B])(input)?;
    let (input, (identifier, auth_code)) = tuple((tag(&[0x4E, 0x45, 0x54, 0x53, 0x43, 0x41, 0x50, 0x45]), tag(&[0x32, 0x2E, 0x30])))(input)?;
    let (input, _) = tag(&[0x00])(input)?;
    Ok((
        input,
        ApplicationExtension {
            identifier: *identifier,
            auth_code: *auth_code,
        },
    ))
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], CommentExtension> {
    let (input, _) = tag(&[0x21, 0xFE])(input)?;
    let (input, comment) = nom::bytes::complete::take_until(&[0x00][..])(input)?;
    let (input, _) = tag(&[0x00])(input)?;
    Ok((
        input,
        CommentExtension {
            comment: comment.to_vec(),
        },
    ))
}

fn parse_blocks(input: &[u8]) -> IResult<&[u8], Vec<Block>> {
    let mut blocks = Vec::new();
    let mut input = input;
    loop {
        if input.is_empty() {
            break;
        }
        match input[0] {
            0x21 => match input[1] {
                0xF9 => {
                    let (new_input, block) = parse_graphic_control_extension(input)?;
                    blocks.push(Block::GraphicControl(block));
                    input = new_input;
                }
                0x01 => {
                    let (new_input, block) = parse_plain_text_extension(input)?;
                    blocks.push(Block::PlainText(block));
                    input = new_input;
                }
                0xFF => {
                    let (new_input, block) = parse_application_extension(input)?;
                    blocks.push(Block::Application(block));
                    input = new_input;
                }
                0xFE => {
                    let (new_input, block) = parse_comment_extension(input)?;
                    blocks.push(Block::Comment(block));
                    input = new_input;
                }
                _ => break,
            },
            _ => break,
        }
    }
    Ok((input, blocks))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;
    let (input, screen_descriptor) = parse_screen_descriptor(input)?;
    let global_color_table = if screen_descriptor.packed_fields & 0x80 != 0 {
        let size = 1 << ((screen_descriptor.packed_fields & 0x07) + 1);
        let (input, table) = parse_color_table(input, size)?;
        Some(table)
    } else {
        None
    };
    let (input, image_descriptor) = parse_image_descriptor(input)?;
    let local_color_table = if image_descriptor.packed_fields & 0x80 != 0 {
        let size = 1 << ((image_descriptor.packed_fields & 0x07) + 1);
        let (input, table) = parse_color_table(input, size)?;
        Some(table)
    } else {
        None
    };
    let (input,