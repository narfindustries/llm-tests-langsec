use nom::{
    bytes::complete::{tag, take as take_bytes},
    combinator::map,
    multi::many0,
    number::complete::{le_u16, le_u8},
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
    global_color_table_flag: bool,
    color_resolution: u8,
    sort_flag: bool,
    size_of_global_color_table: u8,
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
struct ImageDescriptor {
    left_position: u16,
    top_position: u16,
    width: u16,
    height: u16,
    local_color_table_flag: bool,
    interlace_flag: bool,
    sort_flag: bool,
    size_of_local_color_table: u8,
}

#[derive(Debug)]
struct GraphicControlExtension {
    disposal_method: u8,
    user_input_flag: bool,
    transparent_color_flag: bool,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct CommentExtension {
    comment_data: Vec<u8>,
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
    plain_text_data: Vec<u8>,
}

#[derive(Debug)]
struct ApplicationExtension {
    application_identifier: [u8; 8],
    authentication_code: [u8; 3],
    application_data: Vec<u8>,
}

#[derive(Debug)]
struct GifData {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<RGB>>,
    blocks: Vec<Block>,
}

#[derive(Debug)]
enum Block {
    Image {
        descriptor: ImageDescriptor,
        local_color_table: Option<Vec<RGB>>,
        image_data: Vec<u8>,
    },
    GraphicControl(GraphicControlExtension),
    Comment(CommentExtension),
    PlainText(PlainTextExtension),
    Application(ApplicationExtension),
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, (signature, version)) = tuple((
        take_bytes(3usize),
        take_bytes(3usize),
    ))(input)?;

    Ok((input, GifHeader {
        signature: signature.try_into().unwrap(),
        version: version.try_into().unwrap(),
    }))
}

fn parse_packed_field(input: &[u8]) -> IResult<&[u8], (bool, u8, bool, u8)> {
    let (input, packed) = le_u8(input)?;
    Ok((input, (
        (packed & 0x80) != 0,
        ((packed & 0x70) >> 4) as u8,
        (packed & 0x08) != 0,
        (packed & 0x07) as u8,
    )))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height)) = tuple((le_u16, le_u16))(input)?;
    let (input, (global_color_table_flag, color_resolution, sort_flag, size_of_global_color_table)) = parse_packed_field(input)?;
    let (input, (background_color_index, pixel_aspect_ratio)) = tuple((le_u8, le_u8))(input)?;

    Ok((input, LogicalScreenDescriptor {
        width,
        height,
        global_color_table_flag,
        color_resolution,
        sort_flag,
        size_of_global_color_table,
        background_color_index,
        pixel_aspect_ratio,
    }))
}

fn parse_rgb(input: &[u8]) -> IResult<&[u8], RGB> {
    let (input, (r, g, b)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, RGB { r, g, b }))
}

fn parse_color_table(size: u8) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<RGB>> {
    move |input| {
        let count = 1 << (size + 1);
        let mut colors = Vec::with_capacity(count as usize);
        let mut remaining = input;

        for _ in 0..count {
            let (new_input, color) = parse_rgb(remaining)?;
            colors.push(color);
            remaining = new_input;
        }

        Ok((remaining, colors))
    }
}

fn parse_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut data = Vec::new();
    let mut remaining = input;

    loop {
        let (new_input, block_size) = le_u8(remaining)?;
        if block_size == 0 {
            return Ok((new_input, data));
        }

        let (new_input, block_data) = take_bytes(block_size as usize)(new_input)?;
        data.extend_from_slice(block_data);
        remaining = new_input;
    }
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag(&[0x2C])(input)?;
    let (input, (left_position, top_position, width, height)) = 
        tuple((le_u16, le_u16, le_u16, le_u16))(input)?;
    let (input, packed) = le_u8(input)?;

    Ok((input, ImageDescriptor {
        left_position,
        top_position,
        width,
        height,
        local_color_table_flag: (packed & 0x80) != 0,
        interlace_flag: (packed & 0x40) != 0,
        sort_flag: (packed & 0x20) != 0,
        size_of_local_color_table: packed & 0x07,
    }))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tuple((tag(&[0x21]), tag(&[0xF9]), tag(&[0x04])))(input)?;
    let (input, packed) = le_u8(input)?;
    let (input, (delay_time, transparent_color_index)) = tuple((le_u16, le_u8))(input)?;
    let (input, _) = tag(&[0x00])(input)?;

    Ok((input, GraphicControlExtension {
        disposal_method: (packed & 0x1C) >> 2,
        user_input_flag: (packed & 0x02) != 0,
        transparent_color_flag: (packed & 0x01) != 0,
        delay_time,
        transparent_color_index,
    }))
}

fn parse_comment_extension(input: &[u8]) -> IResult<&[u8], CommentExtension> {
    let (input, _) = tuple((tag(&[0x21]), tag(&[0xFE])))(input)?;
    let (input, comment_data) = parse_sub_blocks(input)?;

    Ok((input, CommentExtension { comment_data }))
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], PlainTextExtension> {
    let (input, _) = tuple((tag(&[0x21]), tag(&[0x01]), tag(&[0x0C])))(input)?;
    let (input, (
        text_grid_left_position,
        text_grid_top_position,
        text_grid_width,
        text_grid_height,
        character_cell_width,
        character_cell_height,
        text_foreground_color_index,
        text_background_color_index
    )) = tuple((
        le_u16, le_u16, le_u16, le_u16,
        le_u8, le_u8, le_u8, le_u8
    ))(input)?;
    let (input, plain_text_data) = parse_sub_blocks(input)?;

    Ok((input, PlainTextExtension {
        text_grid_left_position,
        text_grid_top_position,
        text_grid_width,
        text_grid_height,
        character_cell_width,
        character_cell_height,
        text_foreground_color_index,
        text_background_color_index,
        plain_text_data,
    }))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, _) = tuple((tag(&[0x21]), tag(&[0xFF]), tag(&[0x0B])))(input)?;
    let (input, identifier) = take_bytes(8usize)(input)?;
    let (input, auth_code) = take_bytes(3usize)(input)?;
    let (input, application_data) = parse_sub_blocks(input)?;

    Ok((input, ApplicationExtension {
        application_identifier: identifier.try_into().unwrap(),
        authentication_code: auth_code.try_into().unwrap(),
        application_data,
    }))
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, peek_byte) = take_bytes(1usize)(input)?;
    match peek_byte[0] {
        0x2C => {
            let (input, descriptor) = parse_image_descriptor(input)?;
            let (input, local_color_table) = if descriptor.local_color_table_flag {
                let (input, table) = parse_color_table(descriptor.size_of_local_color_table)(input)?;
                (input, Some(table))
            } else {
                (input, None)
            };
            let (input, _) = le_u8(input)?; // LZW minimum code size
            let (input, image_data) = parse_sub_blocks(input)?;
            Ok((input, Block::Image {
                descriptor,
                local_color_table,
                image_data,
            }))
        },
        0x21 => {
            let (_, extension_type) = take_bytes(2usize)(input)?;
            match extension_type[1] {
                0xF9 => map(parse_graphic_control_extension, Block::GraphicControl)(input),
                0xFE => map(parse_comment_extension, Block::Comment)(input),
                0x01 => map(parse_plain_text_extension, Block::PlainText)(input),
                0xFF => map(parse_application_extension, Block::Application)(input),
                _ => panic!("Unknown extension type"),
            }
        },
        _ => panic!("Unknown block type"),
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GifData> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    
    let (input, global_color_table) = if logical_screen_descriptor.global_color_table_flag {
        let (input, table) = parse_color_table(logical_screen_descriptor.size_of_global_color_table)(input)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, blocks) = many0(parse_block)(input)?;
    let (input, _) = tag(&[0x3B])(input)?; // Trailer

    Ok((input, GifData {
        header,
        logical_screen_descriptor,
        global_color_table,
        blocks,
    }))
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
        Ok((_, gif_data)) => println!("{:#?}", gif_data),
        Err(e) => eprintln!("Error parsing GIF: {:?}", e),
    }
}