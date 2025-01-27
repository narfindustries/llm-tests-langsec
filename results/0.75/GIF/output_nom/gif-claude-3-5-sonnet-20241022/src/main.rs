use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u8, u8},
    sequence::tuple,
    IResult,
};
use std::{env, fs};

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 3],
    version: [u8; 3],
    width: u16,
    height: u16,
    packed_fields: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct ColorTableEntry {
    r: u8,
    g: u8,
    b: u8,
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
enum Block {
    Extension(ExtensionBlock),
    Image(ImageBlock),
}

#[derive(Debug)]
struct ImageBlock {
    descriptor: ImageDescriptor,
    local_color_table: Option<Vec<ColorTableEntry>>,
    image_data: Vec<u8>,
}

#[derive(Debug)]
enum ExtensionBlock {
    GraphicControl {
        block_size: u8,
        packed_fields: u8,
        delay_time: u16,
        transparent_color_index: u8,
    },
    Comment(Vec<u8>),
    PlainText {
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
    },
    Application {
        block_size: u8,
        identifier: [u8; 8],
        auth_code: [u8; 3],
        data: Vec<u8>,
    },
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    global_color_table: Option<Vec<ColorTableEntry>>,
    blocks: Vec<Block>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, (signature, version, width, height, packed_fields, background_color_index, pixel_aspect_ratio)) =
        tuple((
            take(3usize),
            take(3usize),
            le_u16,
            le_u16,
            u8,
            u8,
            u8,
        ))(input)?;

    Ok((
        input,
        GifHeader {
            signature: signature.try_into().unwrap(),
            version: version.try_into().unwrap(),
            width,
            height,
            packed_fields,
            background_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_color_table_entry(input: &[u8]) -> IResult<&[u8], ColorTableEntry> {
    let (input, (r, g, b)) = tuple((u8, u8, u8))(input)?;
    Ok((input, ColorTableEntry { r, g, b }))
}

fn parse_color_table(size: usize) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<ColorTableEntry>> {
    move |input| count(parse_color_table_entry, size)(input)
}

fn parse_data_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut result = Vec::new();
    let mut current_input = input;

    loop {
        let (next_input, block_size) = le_u8(current_input)?;
        if block_size == 0 {
            return Ok((next_input, result));
        }

        let (next_input, block_data) = take(block_size as usize)(next_input)?;
        result.extend_from_slice(block_data);
        current_input = next_input;
    }
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (left, top, width, height, packed_fields)) =
        tuple((le_u16, le_u16, le_u16, le_u16, u8))(input)?;

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

fn parse_image_block(input: &[u8]) -> IResult<&[u8], ImageBlock> {
    let (input, _) = tag([0x2C])(input)?;
    let (input, descriptor) = parse_image_descriptor(input)?;

    let has_local_color_table = (descriptor.packed_fields & 0x80) != 0;
    let color_table_size = if has_local_color_table {
        1 << ((descriptor.packed_fields & 0x07) + 1)
    } else {
        0
    };

    let (input, local_color_table) = if has_local_color_table {
        let (input, table) = parse_color_table(color_table_size)(input)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, _lzw_min_code_size) = le_u8(input)?;
    let (input, image_data) = parse_data_sub_blocks(input)?;

    Ok((
        input,
        ImageBlock {
            descriptor,
            local_color_table,
            image_data,
        },
    ))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], ExtensionBlock> {
    let (input, _) = tag([0x21, 0xF9])(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, (packed_fields, delay_time, transparent_color_index, _terminator)) =
        tuple((u8, le_u16, u8, tag([0])))(input)?;

    Ok((
        input,
        ExtensionBlock::GraphicControl {
            block_size,
            packed_fields,
            delay_time,
            transparent_color_index,
        },
    ))
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], ExtensionBlock> {
    let (input, _) = tag([0x21, 0xFE])(input)?;
    let (input, data) = parse_data_sub_blocks(input)?;
    Ok((input, ExtensionBlock::Comment(data)))
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], ExtensionBlock> {
    let (input, _) = tag([0x21, 0x01])(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, (left, top, width, height, cell_width, cell_height, fg_color_index, bg_color_index)) =
        tuple((le_u16, le_u16, le_u16, le_u16, u8, u8, u8, u8))(input)?;
    let (input, data) = parse_data_sub_blocks(input)?;

    Ok((
        input,
        ExtensionBlock::PlainText {
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

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ExtensionBlock> {
    let (input, _) = tag([0x21, 0xFF])(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, identifier) = take(8usize)(input)?;
    let (input, auth_code) = take(3usize)(input)?;
    let (input, data) = parse_data_sub_blocks(input)?;

    Ok((
        input,
        ExtensionBlock::Application {
            block_size,
            identifier: identifier.try_into().unwrap(),
            auth_code: auth_code.try_into().unwrap(),
            data,
        },
    ))
}

fn parse_extension_block(input: &[u8]) -> IResult<&[u8], ExtensionBlock> {
    let (_, next_bytes) = take(2usize)(input)?;
    match next_bytes {
        [0x21, 0xF9] => parse_graphic_control_extension(input),
        [0x21, 0xFE] => parse_comment_extension(input),
        [0x21, 0x01] => parse_plain_text_extension(input),
        [0x21, 0xFF] => parse_application_extension(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (_, first_byte) = take(1usize)(input)?;
    match first_byte[0] {
        0x2C => map(parse_image_block, Block::Image)(input),
        0x21 => map(parse_extension_block, Block::Extension)(input),
        _ => Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Tag,
        ))),
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;

    let has_global_color_table = (header.packed_fields & 0x80) != 0;
    let color_table_size = if has_global_color_table {
        1 << ((header.packed_fields & 0x07) + 1)
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
    let (input, _) = tag([0x3B])(input)?;

    Ok((
        input,
        Gif {
            header,
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

    let data = fs::read(&args[1]).expect("Failed to read file");
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