use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_u16, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};
use std::path::PathBuf;
use clap::Parser;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    #[clap(parse(from_os_str))]
    input_file: PathBuf,
}

#[derive(Debug)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
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
struct ApplicationExtension {
    application_identifier: String,
    application_auth_code: Vec<u8>,
    data: Vec<u8>,
}

#[derive(Debug)]
struct CommentExtension {
    comment_data: String,
}

#[derive(Debug)]
struct PlainTextExtension {
    text_grid_left_position: u16,
    text_grid_top_position: u16,
    text_grid_width: u16,
    text_grid_height: u16,
    character_cell_width: u8,
    character_cell_height: u8,
    text_foreground_color_index: u8,
    text_background_color_index: u8,
    plain_text_data: String,
}

#[derive(Debug)]
enum Block {
    Image(ImageDescriptor, Vec<u8>),
    Extension(u8, Box<dyn std::any::Any>),
}

#[derive(Debug)]
struct Gif {
    header: String,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<Color>>,
    blocks: Vec<Block>,
}

fn parse_color(input: &[u8]) -> IResult<&[u8], Color> {
    map(tuple((le_u8, le_u8, le_u8)), |(red, green, blue)| Color {
        red,
        green,
        blue,
    })(input)
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    map(
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8)),
        |(width, height, packed_fields, background_color_index, pixel_aspect_ratio)| {
            LogicalScreenDescriptor {
                width,
                height,
                packed_fields,
                background_color_index,
                pixel_aspect_ratio,
            }
        },
    )(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    map(
        tuple((tag(b","), le_u16, le_u16, le_u16, le_u16, le_u8)),
        |(_, left_position, top_position, width, height, packed_fields)| ImageDescriptor {
            left_position,
            top_position,
            width,
            height,
            packed_fields,
        },
    )(input)
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    map(
        tuple((tag(b"\x21\xF9\x04"), le_u8, le_u16, le_u8, tag(b"\x00"))),
        |(_, packed_fields, delay_time, transparent_color_index, _)| GraphicControlExtension {
            packed_fields,
            delay_time,
            transparent_color_index,
        },
    )(input)
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, _) = tag(b"\x21\xFF")(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, app_identifier) = map_res(take(8usize), std::str::from_utf8)(input)?;
    let (input, app_auth_code) = take(3usize)(input)?;
    let (input, data) = take(block_size - 11)(input)?;
    Ok((
        input,
        ApplicationExtension {
            application_identifier: app_identifier.to_string(),
            application_auth_code: app_auth_code.to_vec(),
            data: data.to_vec(),
        },
    ))
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], CommentExtension> {
    let (input, _) = tag(b"\x21\xFE")(input)?;
    let (input, size) = le_u8(input)?;
    let (input, comment) = map_res(take(size), std::str::from_utf8)(input)?;
    Ok((
        input,
        CommentExtension {
            comment_data: comment.to_string(),
        },
    ))
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], PlainTextExtension> {
    let (input, _) = tag(b"\x21\x01")(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, (text_grid_left_position, text_grid_top_position, text_grid_width, text_grid_height, character_cell_width, character_cell_height, text_foreground_color_index, text_background_color_index)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u8, le_u8, le_u8, le_u8))(input)?;
    let (input, plain_text_data) = map_res(take(block_size - 12), std::str::from_utf8)(input)?;
    Ok((
        input,
        PlainTextExtension {
            text_grid_left_position,
            text_grid_top_position,
            text_grid_width,
            text_grid_height,
            character_cell_width,
            character_cell_height,
            text_foreground_color_index,
            text_background_color_index,
            plain_text_data: plain_text_data.to_string(),
        },
    ))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, _) = tag(b"GIF")(input)?;
    let (input, version) = map_res(take(3usize), std::str::from_utf8)(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let global_color_table_flag = (logical_screen_descriptor.packed_fields & 0b10000000) != 0;
    let global_color_table_size = 1 << ((logical_screen_descriptor.packed_fields & 0b00000111) + 1);
    let (input, global_color_table) = if global_color_table_flag {
        map(count(parse_color, global_color_table_size as usize), Some)(input)?
    } else {
        (input, None)
    };
    let (input, blocks) = count(
        preceded(
            tag(b"\x21"),
            map(
                tuple((le_u8, le_u8)),
                |(extension_label, block_type)| match block_type {
                    0xF9 => Block::Extension(
                        extension_label,
                        Box::new(parse_graphic_control_extension(input).unwrap().1),
                    ),
                    0x01 => Block::Extension(
                        extension_label,
                        Box::new(parse_plain_text_extension(input).unwrap().1),
                    ),
                    0xFF => Block::Extension(
                        extension_label,
                        Box::new(parse_application_extension(input).unwrap().1),
                    ),
                    0xFE => Block::Extension(
                        extension_label,
                        Box::new(parse_comment_extension(input).unwrap().1),
                    ),
                    _ => unimplemented!(),
                },
            ),
        ),
        1,
    )(input)?;
    let (input, _) = tag(b"\x3B")(input)?;
    Ok((
        input,
        Gif {
            header: format!("GIF{}", version),
            logical_screen_descriptor,
            global_color_table,
            blocks,
        },
    ))
}

fn main() -> io::Result<()> {
    let args = Cli::parse();
    let mut file = File::open(args.input_file)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => println!("Failed to parse GIF: {:?}", e),
    }

    Ok(())
}