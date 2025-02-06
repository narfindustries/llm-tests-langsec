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
struct ColorTable(Vec<RGB>);

#[derive(Debug)]
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_field: u8,
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

fn parse_color_table(size: u8) -> impl Fn(&[u8]) -> IResult<&[u8], ColorTable> {
    move |input| {
        let table_size = 3 * (1 << (size + 1));
        let mut colors = Vec::with_capacity(table_size as usize);
        let mut current_input = input;

        for _ in 0..(table_size / 3) {
            let (new_input, (r, g, b)) = tuple((u8, u8, u8))(current_input)?;
            colors.push(RGB { r, g, b });
            current_input = new_input;
        }

        Ok((current_input, ColorTable(colors)))
    }
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag([0x2C])(input)?;
    let (input, (left, top, width, height, packed_field)) =
        tuple((le_u16, le_u16, le_u16, le_u16, u8))(input)?;
    Ok((
        input,
        ImageDescriptor {
            left,
            top,
            width,
            height,
            packed_field,
        },
    ))
}

fn parse_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut data = Vec::new();
    let mut current_input = input;

    loop {
        let (new_input, block_size) = u8(current_input)?;
        if block_size == 0 {
            return Ok((new_input, data));
        }

        let (new_input, block_data) = take(block_size as usize)(new_input)?;
        data.extend_from_slice(block_data);
        current_input = new_input;
    }
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
    let (input, data) = parse_sub_blocks(input)?;
    Ok((input, CommentExtension { data }))
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], PlainTextExtension> {
    let (input, _) = tag([0x21, 0x01])(input)?;
    let (input, (block_size, text_grid_left, text_grid_top, text_grid_width, text_grid_height, cell_width, cell_height, fg_color_index, bg_color_index)) =
        tuple((u8, le_u16, le_u16, le_u16, le_u16, u8, u8, u8, u8))(input)?;
    let (input, data) = parse_sub_blocks(input)?;
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
            fg_color_index,
            bg_color_index,
            data,
        },
    ))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, _) = tag([0x21, 0xFF])(input)?;
    let (input, block_size) = u8(input)?;
    let (input, identifier) = take(8usize)(input)?;
    let (input, auth_code) = take(3usize)(input)?;
    let (input, data) = parse_sub_blocks(input)?;
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
        let size = lsd.packed_field & 0x07;
        let (input, global_color_table) = parse_color_table(size)(input).unwrap();
        println!("Global Color Table: {:?}", global_color_table);
    }

    Ok(())
}