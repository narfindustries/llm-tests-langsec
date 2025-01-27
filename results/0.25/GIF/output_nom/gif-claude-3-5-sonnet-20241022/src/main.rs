use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u8, u8},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::{env, fs, path::Path};

#[derive(Debug)]
struct GIF {
    header: Header,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<RGB>>,
    blocks: Vec<Block>,
}

#[derive(Debug)]
struct Header {
    version: String,
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    flags: u8,
    bg_color_index: u8,
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
    Extension(Extension),
    ImageDescriptor(ImageDescriptor),
    Trailer,
}

#[derive(Debug)]
enum Extension {
    GraphicControl(GraphicControlExtension),
    Comment(Vec<u8>),
    PlainText(PlainTextExtension),
    Application(ApplicationExtension),
}

#[derive(Debug)]
struct GraphicControlExtension {
    flags: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct PlainTextExtension {
    block_size: u8,
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    cell_width: u8,
    cell_height: u8,
    fg_color_index: u8,
    bg_color_index: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ApplicationExtension {
    block_size: u8,
    identifier: [u8; 8],
    auth_code: [u8; 3],
    data: Vec<u8>,
}

#[derive(Debug)]
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    flags: u8,
    local_color_table: Option<Vec<RGB>>,
    image_data: ImageData,
}

#[derive(Debug)]
struct ImageData {
    lzw_minimum_code_size: u8,
    data: Vec<u8>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, _) = tag("GIF")(input)?;
    let (input, version) = map(take(3u8), |v: &[u8]| {
        String::from_utf8_lossy(v).to_string()
    })(input)?;
    Ok((input, Header { version }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, flags, bg_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, u8, u8, u8))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            flags,
            bg_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_rgb(input: &[u8]) -> IResult<&[u8], RGB> {
    let (input, (r, g, b)) = tuple((u8, u8, u8))(input)?;
    Ok((input, RGB { r, g, b }))
}

fn parse_color_table(size: usize) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<RGB>> {
    move |input| count(parse_rgb, size)(input)
}

fn parse_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut result = Vec::new();
    let mut current_input = input;

    loop {
        let (next_input, block_size) = le_u8(current_input)?;
        if block_size == 0 {
            return Ok((next_input, result));
        }
        let (next_input, block_data) = take(block_size)(next_input)?;
        result.extend_from_slice(block_data);
        current_input = next_input;
    }
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag([0x21, 0xF9])(input)?;
    let (input, _) = tag([0x04])(input)?;
    let (input, (flags, delay_time, transparent_color_index)) =
        tuple((u8, le_u16, u8))(input)?;
    let (input, _) = tag([0x00])(input)?;
    Ok((
        input,
        GraphicControlExtension {
            flags,
            delay_time,
            transparent_color_index,
        },
    ))
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, _) = tag([0x21, 0xFE])(input)?;
    parse_sub_blocks(input)
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], PlainTextExtension> {
    let (input, _) = tag([0x21, 0x01])(input)?;
    let (input, block_size) = u8(input)?;
    let (input, (left, top, width, height, cell_width, cell_height, fg_color_index, bg_color_index)) =
        tuple((le_u16, le_u16, le_u16, le_u16, u8, u8, u8, u8))(input)?;
    let (input, data) = parse_sub_blocks(input)?;
    Ok((
        input,
        PlainTextExtension {
            block_size,
            left,
            top,
            width,
            height,
            cell_width,
            cell_height,
            fg_color_index,
            bg_color_index,
            data,
        },
    ))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, _) = tag([0x21, 0xFF])(input)?;
    let (input, block_size) = u8(input)?;
    let (input, identifier) = map(take(8u8), |v: &[u8]| {
        let mut arr = [0u8; 8];
        arr.copy_from_slice(v);
        arr
    })(input)?;
    let (input, auth_code) = map(take(3u8), |v: &[u8]| {
        let mut arr = [0u8; 3];
        arr.copy_from_slice(v);
        arr
    })(input)?;
    let (input, data) = parse_sub_blocks(input)?;
    Ok((
        input,
        ApplicationExtension {
            block_size,
            identifier,
            auth_code,
            data,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, next_byte) = peek(u8)(input)?;
    match next_byte {
        0x21 => {
            let (input, second_byte) = peek(preceded(tag([0x21]), u8))(input)?;
            match second_byte {
                0xF9 => map(parse_graphic_control_extension, Extension::GraphicControl)(input),
                0xFE => map(parse_comment_extension, Extension::Comment)(input),
                0x01 => map(parse_plain_text_extension, Extension::PlainText)(input),
                0xFF => map(parse_application_extension, Extension::Application)(input),
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

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag([0x2C])(input)?;
    let (input, (left, top, width, height, flags)) =
        tuple((le_u16, le_u16, le_u16, le_u16, u8))(input)?;

    let has_local_color_table = (flags & 0x80) != 0;
    let color_table_size = if has_local_color_table {
        1 << ((flags & 0x07) + 1)
    } else {
        0
    };

    let (input, local_color_table) = if has_local_color_table {
        let (input, table) = parse_color_table(color_table_size)(input)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, lzw_minimum_code_size) = u8(input)?;
    let (input, data) = parse_sub_blocks(input)?;

    Ok((
        input,
        ImageDescriptor {
            left,
            top,
            width,
            height,
            flags,
            local_color_table,
            image_data: ImageData {
                lzw_minimum_code_size,
                data,
            },
        },
    ))
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, next_byte) = peek(u8)(input)?;
    match next_byte {
        0x21 => map(parse_extension, Block::Extension)(input),
        0x2C => map(parse_image_descriptor, Block::ImageDescriptor)(input),
        0x3B => map(tag([0x3B]), |_| Block::Trailer)(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GIF> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let has_global_color_table = (logical_screen_descriptor.flags & 0x80) != 0;
    let color_table_size = if has_global_color_table {
        1 << ((logical_screen_descriptor.flags & 0x07) + 1)
    } else {
        0
    };

    let (input, global_color_table) = if has_global_color_table {
        let (input, table) = parse_color_table(color_table_size)(input)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, blocks) = many0(parse_block)(input)?;

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
        eprintln!("Usage: {} <gif_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let data = fs::read(path).expect("Failed to read file");

    match parse_gif(&data) {
        Ok((remaining, gif)) => {
            if !remaining.is_empty() {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
            println!("{:#?}", gif);
        }
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }
}