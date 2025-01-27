use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{count, many0},
    number::complete::{le_u16, le_u8, u8},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::{env, fs::File, io::Read};

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
    text_grid_left: u16,
    text_grid_top: u16,
    text_grid_width: u16,
    text_grid_height: u16,
    cell_width: u8,
    cell_height: u8,
    text_fg_color_index: u8,
    text_bg_color_index: u8,
    text_data: Vec<u8>,
}

#[derive(Debug)]
struct ApplicationExtension {
    block_size: u8,
    identifier: [u8; 8],
    auth_code: [u8; 3],
    application_data: Vec<u8>,
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
    let (input, version) = map(take(3usize), |v: &[u8]| {
        String::from_utf8_lossy(v).to_string()
    })(input)?;
    Ok((input, Header { version }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, flags, background_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, u8, u8, u8))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            flags,
            background_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_rgb(input: &[u8]) -> IResult<&[u8], RGB> {
    let (input, (r, g, b)) = tuple((u8, u8, u8))(input)?;
    Ok((input, RGB { r, g, b }))
}

fn parse_data_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut result = Vec::new();
    let mut current_input = input;

    loop {
        let (input, block_size) = le_u8(current_input)?;
        if block_size == 0 {
            return Ok((input, result));
        }
        let (input, block_data) = take(block_size as usize)(input)?;
        result.extend_from_slice(block_data);
        current_input = input;
    }
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag([0x21, 0xF9])(input)?;
    let (input, _block_size) = tag([0x04])(input)?;
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
    parse_data_sub_blocks(input)
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], PlainTextExtension> {
    let (input, _) = tag([0x21, 0x01])(input)?;
    let (input, block_size) = u8(input)?;
    let (input, (text_grid_left, text_grid_top, text_grid_width, text_grid_height)) =
        tuple((le_u16, le_u16, le_u16, le_u16))(input)?;
    let (input, (cell_width, cell_height, text_fg_color_index, text_bg_color_index)) =
        tuple((u8, u8, u8, u8))(input)?;
    let (input, text_data) = parse_data_sub_blocks(input)?;
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
            text_fg_color_index,
            text_bg_color_index,
            text_data,
        },
    ))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, _) = tag([0x21, 0xFF])(input)?;
    let (input, block_size) = u8(input)?;
    let (input, identifier) = map(take(8usize), |v: &[u8]| {
        let mut arr = [0u8; 8];
        arr.copy_from_slice(v);
        arr
    })(input)?;
    let (input, auth_code) = map(take(3usize), |v: &[u8]| {
        let mut arr = [0u8; 3];
        arr.copy_from_slice(v);
        arr
    })(input)?;
    let (input, application_data) = parse_data_sub_blocks(input)?;
    Ok((
        input,
        ApplicationExtension {
            block_size,
            identifier,
            auth_code,
            application_data,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension) = nom::branch::alt((
        map(parse_graphic_control_extension, Extension::GraphicControl),
        map(parse_comment_extension, Extension::Comment),
        map(parse_plain_text_extension, Extension::PlainText),
        map(parse_application_extension, Extension::Application),
    ))(input)?;
    Ok((input, extension))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag([0x2C])(input)?;
    let (input, (left, top, width, height, flags)) =
        tuple((le_u16, le_u16, le_u16, le_u16, u8))(input)?;

    let has_local_color_table = (flags & 0x80) != 0;
    let color_table_size = if has_local_color_table {
        2usize.pow((flags & 0x07) as u32 + 1) * 3
    } else {
        0
    };

    let (input, local_color_table) = if color_table_size > 0 {
        let (input, table) = count(parse_rgb, color_table_size / 3)(input)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, lzw_minimum_code_size) = u8(input)?;
    let (input, data) = parse_data_sub_blocks(input)?;

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
    nom::branch::alt((
        map(parse_extension, Block::Extension),
        map(parse_image_descriptor, Block::ImageDescriptor),
        map(tag([0x3B]), |_| Block::Trailer),
    ))(input)
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GIF> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let has_global_color_table = (logical_screen_descriptor.flags & 0x80) != 0;
    let color_table_size = if has_global_color_table {
        2usize.pow((logical_screen_descriptor.flags & 0x07) as u32 + 1) * 3
    } else {
        0
    };

    let (input, global_color_table) = if color_table_size > 0 {
        let (input, table) = count(parse_rgb, color_table_size / 3)(input)?;
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