use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::take_while_m_n,
    number::complete::{be_u16, be_u32},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{BufReader, Read},
};

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 3],
    version: [u8; 3],
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    flags: u8,
    bg_color: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct ColorTable {
    colors: Vec<[u8; 3]>,
}

#[derive(Debug)]
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    flags: u8,
}

#[derive(Debug)]
struct GifImage {
    descriptor: ImageDescriptor,
    data: Vec<u8>,
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<ColorTable>,
    images: Vec<GifImage>,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = take(3u8)(input)?;
    let (input, version) = take(3u8)(input)?;
    Ok((input, GifHeader { signature, version }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, width) = be_u16(input)?;
    let (input, height) = be_u16(input)?;
    let (input, flags) = take(1u8)(input)?;
    let (input, bg_color) = take(1u8)(input)?;
    let (input, pixel_aspect_ratio) = take(1u8)(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            flags: *flags.first().unwrap(),
            bg_color: *bg_color.first().unwrap(),
            pixel_aspect_ratio: *pixel_aspect_ratio.first().unwrap(),
        },
    ))
}

fn parse_color_table(input: &[u8]) -> IResult<&[u8], ColorTable> {
    let (input, colors) = take_while_m_n(3, 768, |x| x.len() % 3 == 0)(input)?;
    let colors: Vec<[u8; 3]> = colors
        .chunks(3)
        .map(|x| [x[0], x[1], x[2]])
        .collect();
    Ok((input, ColorTable { colors }))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, left) = be_u16(input)?;
    let (input, top) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, height) = be_u16(input)?;
    let (input, flags) = take(1u8)(input)?;
    Ok((
        input,
        ImageDescriptor {
            left,
            top,
            width,
            height,
            flags: *flags.first().unwrap(),
        },
    ))
}

fn parse_gif_image(input: &[u8]) -> IResult<&[u8], GifImage> {
    let (input, descriptor) = parse_image_descriptor(input)?;
    let (input, data) = take_while_m_n(1, 65535, |x| x.len() > 0)(input)?;
    Ok((input, GifImage { descriptor, data: data.to_vec() }))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_gif_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let (input, global_color_table) = opt(parse_color_table)(input)?;
    let mut images = Vec::new();
    let mut input = input;
    while !input.is_empty() {
        let (new_input, image) = parse_gif_image(input)?;
        images.push(image);
        input = new_input;
    }
    Ok((
        input,
        Gif {
            header,
            logical_screen_descriptor,
            global_color_table,
            images,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Usage: {} <input_file>", args[0]);
    }
    let file = File::open(&args[1]).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let (_rest, gif) = parse_gif(&input).unwrap();
    println!("{:?}", gif);
}