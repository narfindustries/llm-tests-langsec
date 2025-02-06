use nom::{
    bytes::complete::{tag, take},
    multi::{count, many0},
    number::complete::{le_u8, le_u16},
    sequence::{tuple, preceded},
    IResult,
    branch::alt,
    combinator::{map, opt, cond},
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
struct GraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    logical_screen: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    images: Vec<GifImage>,
}

#[derive(Debug)]
struct GifImage {
    descriptor: ImageDescriptor,
    local_color_table: Option<Vec<ColorTableEntry>>,
    graphic_control: Option<GraphicControlExtension>,
    lzw_min_code_size: u8,
    image_data: Vec<u8>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = take(3usize)(input)?;
    let (input, version) = take(3usize)(input)?;
    
    Ok((input, GifHeader {
        signature: signature.try_into().unwrap(),
        version: version.try_into().unwrap(),
    }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, background_color_index) = le_u8(input)?;
    let (input, pixel_aspect_ratio) = le_u8(input)?;

    Ok((input, LogicalScreenDescriptor {
        width,
        height,
        packed_fields,
        background_color_index,
        pixel_aspect_ratio,
    }))
}

fn parse_color_table(input: &[u8], table_size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    count(
        map(tuple((le_u8, le_u8, le_u8)), |(r, g, b)| ColorTableEntry { r, g, b }),
        table_size
    )(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag([0x2C])(input)?;
    let (input, left) = le_u16(input)?;
    let (input, top) = le_u16(input)?;
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;

    Ok((input, ImageDescriptor {
        left,
        top,
        width,
        height,
        packed_fields,
    }))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag([0x21, 0xF9])(input)?;
    let (input, _block_size) = tag([4])(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, delay_time) = le_u16(input)?;
    let (input, transparent_color_index) = le_u8(input)?;
    let (input, _) = tag([0])(input)?;

    Ok((input, GraphicControlExtension {
        packed_fields,
        delay_time,
        transparent_color_index,
    }))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], (u8, Vec<u8>)> {
    let (input, lzw_min_code_size) = le_u8(input)?;
    let (input, data_blocks) = many0(preceded(le_u8, take_while_m_n))(input)?;
    
    Ok((input, (lzw_min_code_size, data_blocks.concat())))
}

fn take_while_m_n(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, block) = take(input[0] as usize)(input)?;
    Ok((input, block.to_vec()))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen) = parse_logical_screen_descriptor(input)?;

    let (input, global_color_table) = if logical_screen.packed_fields & 0x80 != 0 {
        let table_size = 2 << (logical_screen.packed_fields & 0x07);
        let (input, table) = parse_color_table(input, table_size)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, images) = many0(parse_gif_image)(input)?;
    let (input, _) = tag([0x3B])(input)?;

    Ok((input, Gif {
        header,
        logical_screen,
        global_color_table,
        images,
    }))
}

fn parse_gif_image(input: &[u8]) -> IResult<&[u8], GifImage> {
    let (input, graphic_control) = opt(parse_graphic_control_extension)(input)?;
    let (input, descriptor) = parse_image_descriptor(input)?;

    let (input, local_color_table) = if descriptor.packed_fields & 0x80 != 0 {
        let table_size = 2 << (descriptor.packed_fields & 0x07);
        let (input, table) = parse_color_table(input, table_size)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let (input, (lzw_min_code_size, image_data)) = parse_image_data(input)?;

    Ok((input, GifImage {
        descriptor,
        local_color_table,
        graphic_control,
        lzw_min_code_size,
        image_data,
    }))
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
            println!("Parsed GIF: {:?}", gif);
            Ok(())
        }
        Err(e) => {
            eprintln!("Failed to parse GIF: {:?}", e);
            std::process::exit(1);
        }
    }
}