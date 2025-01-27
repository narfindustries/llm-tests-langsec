use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt},
    multi::{count, many0},
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
struct GifHeader {
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
    entries: Vec<(u8, u8, u8)>,
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
struct ImageData {
    lzw_minimum_code_size: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct GifFrame {
    image_descriptor: ImageDescriptor,
    local_color_table: Option<ColorTable>,
    image_data: ImageData,
}

#[derive(Debug)]
struct GifFile {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<ColorTable>,
    frames: Vec<GifFrame>,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = map_res(take(3usize), |s: &[u8]| std::str::from_utf8(s).map(|s| s.to_string()))(input)?;
    let (input, version) = map_res(take(3usize), |s: &[u8]| std::str::from_utf8(s).map(|s| s.to_string()))(input)?;
    Ok((input, GifHeader { signature, version }))
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
    let (input, entries) = count(map(tuple((le_u8, le_u8, le_u8)), |(r, g, b)| (r, g, b)), size)(input)?;
    Ok((input, ColorTable { entries }))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (left, top, width, height, packed_fields)) = tuple((le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
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

fn parse_image_data(input: &[u8]) -> IResult<&[u8], ImageData> {
    let (input, lzw_minimum_code_size) = le_u8(input)?;
    let (input, data) = many0(preceded(le_u8, take(usize::from)))(input)?;
    Ok((
        input,
        ImageData {
            lzw_minimum_code_size,
            data: data.into_iter().flatten().copied().collect(),
        },
    ))
}

fn parse_gif_frame(input: &[u8]) -> IResult<&[u8], GifFrame> {
    let (input, image_descriptor) = parse_image_descriptor(input)?;
    let (input, local_color_table) = if image_descriptor.packed_fields & 0x80 != 0 {
        let size = 1 << ((image_descriptor.packed_fields & 0x07) + 1);
        map(parse_color_table, Some)(input, size)?
    } else {
        (input, None)
    };
    let (input, image_data) = parse_image_data(input)?;
    Ok((
        input,
        GifFrame {
            image_descriptor,
            local_color_table,
            image_data,
        },
    ))
}

fn parse_gif_file(input: &[u8]) -> IResult<&[u8], GifFile> {
    let (input, header) = parse_gif_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let (input, global_color_table) = if logical_screen_descriptor.packed_fields & 0x80 != 0 {
        let size = 1 << ((logical_screen_descriptor.packed_fields & 0x07) + 1);
        map(parse_color_table, Some)(input, size)?
    } else {
        (input, None)
    };
    let (input, frames) = many0(parse_gif_frame)(input)?;
    Ok((
        input,
        GifFile {
            header,
            logical_screen_descriptor,
            global_color_table,
            frames,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <gif file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif_file(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }

    Ok(())
}