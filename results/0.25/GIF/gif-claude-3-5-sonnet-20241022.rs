use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 3],
    version: [u8; 3],
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed_field: u8,
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
struct GlobalColorTable(Vec<RGB>);

#[derive(Debug)]
struct ImageDescriptor {
    left_position: u16,
    top_position: u16,
    width: u16,
    height: u16,
    packed_field: u8,
}

#[derive(Debug)]
struct LocalColorTable(Vec<RGB>);

#[derive(Debug)]
struct ImageData {
    lzw_minimum_code_size: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct GraphicControlExtension {
    block_size: u8,
    packed_field: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct CommentExtension {
    data: Vec<u8>,
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
    text_data: Vec<u8>,
}

#[derive(Debug)]
struct ApplicationExtension {
    block_size: u8,
    identifier: [u8; 8],
    auth_code: [u8; 3],
    data: Vec<u8>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, (signature, version)) = tuple((take(3usize), take(3usize)))(input)?;
    Ok((
        input,
        GifHeader {
            signature: signature.try_into().unwrap(),
            version: version.try_into().unwrap(),
        },
    ))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_field, background_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, u8, u8, u8))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            packed_field,
            background_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<RGB>> {
    let mut colors = Vec::with_capacity(size);
    let mut remaining = input;

    for _ in 0..size {
        let (input, (r, g, b)) = tuple((u8, u8, u8))(remaining)?;
        colors.push(RGB { r, g, b });
        remaining = input;
    }

    Ok((remaining, colors))
}

fn parse_data_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut data = Vec::new();
    let mut remaining = input;

    loop {
        let (input, block_size) = u8(remaining)?;
        if block_size == 0 {
            return Ok((input, data));
        }

        let (input, block_data) = take(block_size as usize)(input)?;
        data.extend_from_slice(block_data);
        remaining = input;
    }
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag([0x2C])(input)?;
    let (input, (left, top, width, height, packed_field)) =
        tuple((le_u16, le_u16, le_u16, le_u16, u8))(input)?;

    Ok((
        input,
        ImageDescriptor {
            left_position: left,
            top_position: top,
            width,
            height,
            packed_field,
        },
    ))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], ImageData> {
    let (input, lzw_minimum_code_size) = u8(input)?;
    let (input, data) = parse_data_sub_blocks(input)?;

    Ok((
        input,
        ImageData {
            lzw_minimum_code_size,
            data,
        },
    ))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag([0x21, 0xF9])(input)?;
    let (input, (block_size, packed_field, delay_time, transparent_color_index, _)) =
        tuple((u8, u8, le_u16, u8, u8))(input)?;

    Ok((
        input,
        GraphicControlExtension {
            block_size,
            packed_field,
            delay_time,
            transparent_color_index,
        },
    ))
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], CommentExtension> {
    let (input, _) = tag([0x21, 0xFE])(input)?;
    let (input, data) = parse_data_sub_blocks(input)?;

    Ok((input, CommentExtension { data }))
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], PlainTextExtension> {
    let (input, _) = tag([0x21, 0x01])(input)?;
    let (input, block_size) = u8(input)?;
    let (input, (grid_left, grid_top, grid_width, grid_height)) =
        tuple((le_u16, le_u16, le_u16, le_u16))(input)?;
    let (input, (cell_width, cell_height, fg_color, bg_color)) =
        tuple((u8, u8, u8, u8))(input)?;
    let (input, text_data) = parse_data_sub_blocks(input)?;

    Ok((
        input,
        PlainTextExtension {
            block_size,
            text_grid_left: grid_left,
            text_grid_top: grid_top,
            text_grid_width: grid_width,
            text_grid_height: grid_height,
            cell_width,
            cell_height,
            foreground_color: fg_color,
            background_color: bg_color,
            text_data,
        },
    ))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, _) = tag([0x21, 0xFF])(input)?;
    let (input, block_size) = u8(input)?;
    let (input, identifier) = take(8usize)(input)?;
    let (input, auth_code) = take(3usize)(input)?;
    let (input, data) = parse_data_sub_blocks(input)?;

    Ok((
        input,
        ApplicationExtension {
            block_size,
            identifier: identifier.try_into().unwrap(),
            auth_code: auth_code.try_into().unwrap(),
            data,
        },
    ))
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

    let (input, header) = parse_header(&buffer).unwrap();
    println!("Header: {:?}", header);

    let (input, lsd) = parse_logical_screen_descriptor(input).unwrap();
    println!("Logical Screen Descriptor: {:?}", lsd);

    if lsd.packed_field & 0x80 != 0 {
        let size = 3 * (1 << ((lsd.packed_field & 0x07) + 1));
        let (input, global_color_table) = parse_color_table(input, size).unwrap();
        println!("Global Color Table: {} colors", global_color_table.len());
    }

    Ok(())
}