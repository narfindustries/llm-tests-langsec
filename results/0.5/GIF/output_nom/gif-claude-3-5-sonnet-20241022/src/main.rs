use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{length_count, many0, many1},
    number::complete::{le_u16, le_u8},
    sequence::{pair, preceded, terminated, tuple},
    IResult,
};
use std::{env, fs::File, io::Read, path::Path};

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
    image_data: Vec<u8>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    let (input, _) = tag("GIF")(input)?;
    let (input, version) = alt((tag("87a"), tag("89a")))(input)?;
    Ok((
        input,
        Header {
            version: String::from_utf8_lossy(version).to_string(),
        },
    ))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, flags, background_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
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
    let (input, (r, g, b)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, RGB { r, g, b }))
}

fn parse_color_table(size: usize) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<RGB>> {
    move |input| count!(input, parse_rgb, size)
}

fn parse_data_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (mut input, mut result) = (input, Vec::new());
    loop {
        let (new_input, block_size) = le_u8(input)?;
        if block_size == 0 {
            return Ok((new_input, result));
        }
        let (new_input, block_data) = take(block_size as usize)(new_input)?;
        result.extend_from_slice(block_data);
        input = new_input;
    }
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag([0x21, 0xF9])(input)?;
    let (input, _) = tag([0x04])(input)?;
    let (input, (flags, delay_time, transparent_color_index)) =
        tuple((le_u8, le_u16, le_u8))(input)?;
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
    let (input, _) = tag([0x0C])(input)?;
    let (input, (text_grid_left, text_grid_top, text_grid_width, text_grid_height)) =
        tuple((le_u16, le_u16, le_u16, le_u16))(input)?;
    let (input, (cell_width, cell_height, text_fg_color_index, text_bg_color_index)) =
        tuple((le_u8, le_u8, le_u8, le_u8))(input)?;
    let (input, text_data) = parse_data_sub_blocks(input)?;
    Ok((
        input,
        PlainTextExtension {
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
    let (input, _) = tag([0x0B])(input)?;
    let (input, identifier) = take(8usize)(input)?;
    let (input, auth_code) = take(3usize)(input)?;
    let (input, data) = parse_data_sub_blocks(input)?;
    Ok((
        input,
        ApplicationExtension {
            identifier: identifier.try_into().unwrap(),
            auth_code: auth_code.try_into().unwrap(),
            data,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    alt((
        map(parse_graphic_control_extension, Extension::GraphicControl),
        map(parse_comment_extension, Extension::Comment),
        map(parse_plain_text_extension, Extension::PlainText),
        map(parse_application_extension, Extension::Application),
    ))(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag([0x2C])(input)?;
    let (input, (left, top, width, height, flags)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
    
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

    let (input, _lzw_min_code_size) = le_u8(input)?;
    let (input, image_data) = parse_data_sub_blocks(input)?;

    Ok((
        input,
        ImageDescriptor {
            left,
            top,
            width,
            height,
            flags,
            local_color_table,
            image_data,
        },
    ))
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    alt((
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

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gif-file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((remaining, gif)) => {
            if !remaining.is_empty() {
                println!("Warning: {} unparsed bytes remaining", remaining.len());
            }
            println!("{:#?}", gif);
        }
        Err(e) => eprintln!("Error parsing GIF: {:?}", e),
    }

    Ok(())
}