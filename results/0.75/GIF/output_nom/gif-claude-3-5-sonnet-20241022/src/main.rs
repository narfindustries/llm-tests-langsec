use nom::bytes::complete::{tag, take};
use nom::combinator::{map, opt};
use nom::multi::many0;
use nom::number::complete::{le_u16, u8};
use nom::sequence::tuple;
use nom::IResult;
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct GIF {
    header: Header,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<RGB>>,
    blocks: Vec<Block>,
}

#[derive(Debug)]
struct Header {
    signature: [u8; 3],
    version: [u8; 3],
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct RGB {
    r: u8,
    g: u8,
    b: u8,
}

#[derive(Debug)]
enum Block {
    ImageDescriptor(ImageDescriptor),
    GraphicControlExtension(GraphicControlExtension),
    CommentExtension(CommentExtension),
    PlainTextExtension(PlainTextExtension),
    ApplicationExtension(ApplicationExtension),
}

#[derive(Debug)]
struct ImageDescriptor {
    separator: u8,
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed: u8,
    local_color_table: Option<Vec<RGB>>,
    image_data: ImageData,
}

#[derive(Debug)]
struct ImageData {
    lzw_minimum_code_size: u8,
    data_sub_blocks: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct GraphicControlExtension {
    block_size: u8,
    packed: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct CommentExtension {
    data_sub_blocks: Vec<Vec<u8>>,
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
    foreground_color: u8,
    background_color: u8,
    data_sub_blocks: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct ApplicationExtension {
    block_size: u8,
    identifier: [u8; 8],
    auth_code: [u8; 3],
    data_sub_blocks: Vec<Vec<u8>>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    map(
        tuple((tag("GIF"), take(3usize))),
        |(sig, ver): (&[u8], &[u8])| Header {
            signature: sig.try_into().unwrap(),
            version: ver.try_into().unwrap(),
        },
    )(input)
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    map(
        tuple((le_u16, le_u16, u8, u8, u8)),
        |(width, height, packed, bg_color, aspect)| LogicalScreenDescriptor {
            width,
            height,
            packed,
            background_color_index: bg_color,
            pixel_aspect_ratio: aspect,
        },
    )(input)
}

fn parse_rgb(input: &[u8]) -> IResult<&[u8], RGB> {
    map(tuple((u8, u8, u8)), |(r, g, b)| RGB { r, g, b })(input)
}

fn parse_color_table(size: u8) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<RGB>> {
    move |input| {
        let count = 1 << (size + 1);
        nom::multi::count(parse_rgb, count as usize)(input)
    }
}

fn parse_data_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<Vec<u8>>> {
    let mut result = Vec::new();
    let mut remaining = input;

    loop {
        let (next_input, block_size) = u8(remaining)?;
        if block_size == 0 {
            return Ok((next_input, result));
        }

        let (next_input, block_data) = take(block_size as usize)(next_input)?;
        result.push(block_data.to_vec());
        remaining = next_input;
    }
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, separator) = tag([0x2C])(input)?;
    let (input, (left, top, width, height, packed)) =
        tuple((le_u16, le_u16, le_u16, le_u16, u8))(input)?;

    let has_local_table = (packed & 0x80) != 0;
    let table_size = packed & 0x07;

    let (input, local_color_table) = if has_local_table {
        let (input, table) = parse_color_table(table_size)(input)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, lzw_minimum_code_size) = u8(input)?;
    let (input, data_sub_blocks) = parse_data_sub_blocks(input)?;

    Ok((
        input,
        ImageDescriptor {
            separator: separator[0],
            left,
            top,
            width,
            height,
            packed,
            local_color_table,
            image_data: ImageData {
                lzw_minimum_code_size,
                data_sub_blocks,
            },
        },
    ))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag([0x21, 0xF9])(input)?;
    let (input, (block_size, packed, delay_time, transparent_color_index, _)) =
        tuple((u8, u8, le_u16, u8, tag([0x00])))(input)?;

    Ok((
        input,
        GraphicControlExtension {
            block_size,
            packed,
            delay_time,
            transparent_color_index,
        },
    ))
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], CommentExtension> {
    let (input, _) = tag([0x21, 0xFE])(input)?;
    let (input, data_sub_blocks) = parse_data_sub_blocks(input)?;

    Ok((input, CommentExtension { data_sub_blocks }))
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], PlainTextExtension> {
    let (input, _) = tag([0x21, 0x01])(input)?;
    let (input, block_size) = u8(input)?;
    let (input, (text_grid_left, text_grid_top, text_grid_width, text_grid_height)) =
        tuple((le_u16, le_u16, le_u16, le_u16))(input)?;
    let (input, (cell_width, cell_height, foreground_color, background_color)) =
        tuple((u8, u8, u8, u8))(input)?;
    let (input, data_sub_blocks) = parse_data_sub_blocks(input)?;

    Ok((
        input,
        PlainTextExtension {
            block_size,
            text_grid_left,
            text_grid_top,
            text_grid_width,
            text_grid_height,
            cell_width,
            cell_height,
            foreground_color,
            background_color,
            data_sub_blocks,
        },
    ))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, _) = tag([0x21, 0xFF])(input)?;
    let (input, block_size) = u8(input)?;
    let (input, identifier) = take(8usize)(input)?;
    let (input, auth_code) = take(3usize)(input)?;
    let (input, data_sub_blocks) = parse_data_sub_blocks(input)?;

    Ok((
        input,
        ApplicationExtension {
            block_size,
            identifier: identifier.try_into().unwrap(),
            auth_code: auth_code.try_into().unwrap(),
            data_sub_blocks,
        },
    ))
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (_, peek_byte) = nom::number::complete::u8(input)?;
    match peek_byte {
        0x2C => map(parse_image_descriptor, Block::ImageDescriptor)(input),
        0x21 => {
            let (_, extension_type) = nom::number::complete::u8(&input[1..])?;
            match extension_type {
                0xF9 => map(parse_graphic_control_extension, Block::GraphicControlExtension)(input),
                0xFE => map(parse_comment_extension, Block::CommentExtension)(input),
                0x01 => map(parse_plain_text_extension, Block::PlainTextExtension)(input),
                0xFF => map(parse_application_extension, Block::ApplicationExtension)(input),
                _ => Err(nom::Err::Error(nom::error::Error::new(
                    input,
                    nom::error::ErrorKind::Tag,
                ))),
            }
        }
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GIF> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let has_global_table = (logical_screen_descriptor.packed & 0x80) != 0;
    let global_table_size = logical_screen_descriptor.packed & 0x07;

    let (input, global_color_table) = if has_global_table {
        let (input, table) = parse_color_table(global_table_size)(input)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, blocks) = many0(parse_block)(input)?;
    let (input, _) = tag([0x3B])(input)?;

    Ok((
        input,
        GIF {
            header,
            logical_screen_descriptor,
            global_color_table,
            blocks,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gif-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    match parse_gif(&buffer) {
        Ok((remaining, gif)) => {
            if !remaining.is_empty() {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
            println!("{:#?}", gif);
        }
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }
}