use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    bytes::complete::{tag, take},
    multi::{count, many0},
    number::complete::{le_u8, le_u16, le_u32},
    sequence::{tuple, preceded},
    combinator::{map, opt, cond}
};

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
    separator: u8,
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct ImageData {
    lzw_code_size: u8,
    data_blocks: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct GifExtension {
    label: u8,
    data: ExtensionData,
}

#[derive(Debug)]
enum ExtensionData {
    GraphicControl(GraphicControlExtension),
    Comment(CommentExtension),
    PlainText(PlainTextExtension),
    Application(ApplicationExtension),
}

#[derive(Debug)]
struct GraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct CommentExtension {
    comments: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct PlainTextExtension {
    text_grid_left: u16,
    text_grid_top: u16,
    text_grid_width: u16,
    text_grid_height: u16,
    cell_width: u8,
    cell_height: u8,
    foreground_color_index: u8,
    background_color_index: u8,
    text_data: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct ApplicationExtension {
    identifier: [u8; 8],
    authentication_code: [u8; 3],
    data_blocks: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    blocks: Vec<GifBlock>,
    trailer: u8,
}

#[derive(Debug)]
enum GifBlock {
    ImageBlock {
        image_descriptor: ImageDescriptor,
        local_color_table: Option<Vec<ColorTableEntry>>,
        image_data: ImageData,
    },
    Extension(GifExtension),
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    map(
        tuple((
            take(3usize),
            take(3usize),
        )),
        |(signature, version)| GifHeader {
            signature: signature.try_into().unwrap(),
            version: version.try_into().unwrap(),
        }
    )(input)
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    map(
        tuple((
            le_u16,
            le_u16,
            le_u8,
            le_u8,
            le_u8,
        )),
        |(width, height, packed_fields, background_color_index, pixel_aspect_ratio)| LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
            background_color_index,
            pixel_aspect_ratio,
        }
    )(input)
}

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    count(
        map(
            tuple((le_u8, le_u8, le_u8)),
            |(red, green, blue)| ColorTableEntry { red, green, blue }
        ),
        size
    )(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    map(
        tuple((
            le_u8,
            le_u16,
            le_u16,
            le_u16,
            le_u16,
            le_u8,
        )),
        |(separator, left, top, width, height, packed_fields)| ImageDescriptor {
            separator,
            left,
            top,
            width,
            height,
            packed_fields,
        }
    )(input)
}

fn parse_data_block(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    preceded(
        le_u8,
        take(input[0] as usize)
    )(input)
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], ImageData> {
    map(
        tuple((
            le_u8,
            many0(parse_data_block),
        )),
        |(lzw_code_size, data_blocks)| ImageData {
            lzw_code_size,
            data_blocks,
        }
    )(input)
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    map(
        tuple((
            le_u8,
            le_u8,
            le_u16,
            le_u8,
            tag(&[0x00]),
        )),
        |(label, packed_fields, delay_time, transparent_color_index, _)| GraphicControlExtension {
            packed_fields,
            delay_time,
            transparent_color_index,
        }
    )(input)
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], CommentExtension> {
    map(
        many0(parse_data_block),
        |comments| CommentExtension { comments }
    )(input)
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], PlainTextExtension> {
    map(
        tuple((
            le_u16, le_u16, le_u16, le_u16,
            le_u8, le_u8,
            le_u8, le_u8,
            many0(parse_data_block),
            tag(&[0x00]),
        )),
        |(text_grid_left, text_grid_top, text_grid_width, text_grid_height,
          cell_width, cell_height,
          foreground_color_index, background_color_index,
          text_data, _)| PlainTextExtension {
            text_grid_left,
            text_grid_top,
            text_grid_width,
            text_grid_height,
            cell_width,
            cell_height,
            foreground_color_index,
            background_color_index,
            text_data,
        }
    )(input)
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    map(
        tuple((
            take(8usize),
            take(3usize),
            many0(parse_data_block),
            tag(&[0x00]),
        )),
        |(identifier, authentication_code, data_blocks, _)| ApplicationExtension {
            identifier: identifier.try_into().unwrap(),
            authentication_code: authentication_code.try_into().unwrap(),
            data_blocks,
        }
    )(input)
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], GifExtension> {
    map(
        tuple((
            tag(&[0x21]),
            le_u8,
            map_extension,
        )),
        |(_, label, data)| GifExtension { label, data }
    )(input)
}

fn map_extension(input: &[u8]) -> IResult<&[u8], ExtensionData> {
    match input[0] {
        0xF9 => map(parse_graphic_control_extension, ExtensionData::GraphicControl)(input),
        0xFE => map(parse_comment_extension, ExtensionData::Comment)(input),
        0x01 => map(parse_plain_text_extension, ExtensionData::PlainText)(input),
        0xFF => map(parse_application_extension, ExtensionData::Application)(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_gif_block(input: &[u8]) -> IResult<&[u8], GifBlock> {
    alt!(input,
        map(parse_extension, GifBlock::Extension) |
        map(
            tuple((
                parse_image_descriptor,
                opt(|i| parse_color_table(i, color_table_size_from_packed_fields(i[0]))),
                parse_image_data,
            )),
            |(image_descriptor, local_color_table, image_data)| GifBlock::ImageBlock {
                image_descriptor,
                local_color_table,
                image_data,
            }
        )
    )
}

fn color_table_size_from_packed_fields(packed_fields: u8) -> usize {
    let color_table_flag = (packed_fields & 0x80) >> 7;
    let color_table_size = (packed_fields & 0x07) + 1;
    if color_table_flag == 1 {
        2usize.pow(color_table_size as u32)
    } else {
        0
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    map(
        tuple((
            parse_gif_header,
            parse_logical_screen_descriptor,
            opt(|i| parse_color_table(i, color_table_size_from_packed_fields(i[4]))),
            many0(parse_gif_block),
            tag(&[0x3B]),
        )),
        |(header, screen_descriptor, global_color_table, blocks, trailer)| Gif {
            header,
            screen_descriptor,
            global_color_table,
            blocks,
            trailer: trailer[0],
        }
    )(input)
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
            println!("Successfully parsed GIF: {:?}", gif);
            Ok(())
        },
        Err(e) => {
            eprintln!("Failed to parse GIF: {:?}", e);
            std::process::exit(1);
        }
    }
}