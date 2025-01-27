use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::{count, length_data},
    number::complete::{le_u16, le_u8},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct GIFHeader {
    signature: String,
    version: String,
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
struct ColorTable {
    colors: Vec<(u8, u8, u8)>,
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
struct GIF {
    header: GIFHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<ColorTable>,
    blocks: Vec<Block>,
}

#[derive(Debug)]
enum Block {
    Image(ImageDescriptor, Option<ColorTable>, Vec<u8>),
    Extension(Extension),
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
    disposal_method: u8,
    user_input_flag: bool,
    transparent_color_flag: bool,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct PlainTextExtension {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    cell_width: u8,
    cell_height: u8,
    foreground_color_index: u8,
    background_color_index: u8,
    text: Vec<u8>,
}

#[derive(Debug)]
struct ApplicationExtension {
    identifier: [u8; 8],
    authentication_code: [u8; 3],
    data: Vec<u8>,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GIFHeader> {
    let (input, signature) = map_res(take(3usize), |s: &[u8]| String::from_utf8(s.to_vec()))(input)?;
    let (input, version) = map_res(take(3usize), |s: &[u8]| String::from_utf8(s.to_vec()))(input)?;
    Ok((input, GIFHeader { signature, version }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
            background_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], ColorTable> {
    let (input, colors) = count(map(tuple((le_u8, le_u8, le_u8)), |(r, g, b)| (r, g, b)), size)(input)?;
    Ok((input, ColorTable { colors }))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (left, top, width, height, packed_fields)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
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

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, (packed_fields, delay_time, transparent_color_index)) =
        tuple((le_u8, le_u16, le_u8))(input)?;
    Ok((
        input,
        GraphicControlExtension {
            disposal_method: (packed_fields >> 2) & 0x07,
            user_input_flag: (packed_fields & 0x02) != 0,
            transparent_color_flag: (packed_fields & 0x01) != 0,
            delay_time,
            transparent_color_index,
        },
    ))
}

fn parse_plain_text_extension(input: &[u8]) -> IResult<&[u8], PlainTextExtension> {
    let (input, (left, top, width, height, cell_width, cell_height, foreground_color_index, background_color_index)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u8, le_u8, le_u8, le_u8))(input)?;
    let (input, text) = length_data(le_u8)(input)?;
    Ok((
        input,
        PlainTextExtension {
            left,
            top,
            width,
            height,
            cell_width,
            cell_height,
            foreground_color_index,
            background_color_index,
            text: text.to_vec(),
        },
    ))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, identifier) = map(take(8usize), |s: &[u8]| {
        let mut arr = [0; 8];
        arr.copy_from_slice(s);
        arr
    })(input)?;
    let (input, authentication_code) = map(take(3usize), |s: &[u8]| {
        let mut arr = [0; 3];
        arr.copy_from_slice(s);
        arr
    })(input)?;
    let (input, data) = length_data(le_u8)(input)?;
    Ok((
        input,
        ApplicationExtension {
            identifier,
            authentication_code,
            data: data.to_vec(),
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, extension_type) = le_u8(input)?;
    match extension_type {
        0xF9 => {
            let (input, gce) = parse_graphic_control_extension(input)?;
            Ok((input, Extension::GraphicControl(gce)))
        }
        0xFE => {
            let (input, comment) = length_data(le_u8)(input)?;
            Ok((input, Extension::Comment(comment.to_vec())))
        }
        0x01 => {
            let (input, pte) = parse_plain_text_extension(input)?;
            Ok((input, Extension::PlainText(pte)))
        }
        0xFF => {
            let (input, ae) = parse_application_extension(input)?;
            Ok((input, Extension::Application(ae)))
        }
        _ => Err(nom::Err::Error((input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, block_type) = le_u8(input)?;
    match block_type {
        0x2C => {
            let (input, image_descriptor) = parse_image_descriptor(input)?;
            let (input, local_color_table) = if (image_descriptor.packed_fields & 0x80) != 0 {
                let size = 1 << ((image_descriptor.packed_fields & 0x07) + 1);
                let (input, ct) = parse_color_table(input, size)?;
                (input, Some(ct))
            } else {
                (input, None)
            };
            let (input, image_data) = length_data(le_u8)(input)?;
            Ok((input, Block::Image(image_descriptor, local_color_table, image_data.to_vec())))
        }
        0x21 => {
            let (input, extension) = parse_extension(input)?;
            Ok((input, Block::Extension(extension)))
        }
        0x3B => Ok((input, Block::Trailer)),
        _ => Err(nom::Err::Error((input, nom::error::ErrorKind::Tag))),
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GIF> {
    let (input, header) = parse_gif_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let (input, global_color_table) = if (logical_screen_descriptor.packed_fields & 0x80) != 0 {
        let size = 1 << ((logical_screen_descriptor.packed_fields & 0x07) + 1);
        let (input, ct) = parse_color_table(input, size)?;
        (input, Some(ct))
    } else {
        (input, None)
    };
    let (input, blocks) = nom::multi::many_till(parse_block, tag(&[0x3B]))(input)?;
    Ok((
        input,
        GIF {
            header,
            logical_screen_descriptor,
            global_color_table,
            blocks: blocks.0,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gif file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }

    Ok(())
}