use nom::{
    bytes::complete::{take, take_while},
    combinator::{map},
    number::complete::{be_u16, be_u8},
    sequence::{tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct Gif {
    signature: String,
    version: String,
    logical_screen_width: u16,
    logical_screen_height: u16,
    packed_fields: PackedFields,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
    global_color_table: Option<Vec<Color>>,
    images: Vec<Image>,
}

#[derive(Debug)]
struct PackedFields {
    global_color_table_size: u8,
    sort_flag: bool,
    color_resolution: u8,
    global_color_table_flag: bool,
}

#[derive(Debug, PartialEq)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
}

#[derive(Debug)]
struct Image {
    image_separator: u8,
    image_left_position: u16,
    image_top_position: u16,
    image_width: u16,
    image_height: u16,
    packed_fields: PackedFields,
    local_color_table: Option<Vec<Color>>,
    image_data: Vec<u8>,
}

fn parse_signature(input: &[u8]) -> IResult<&[u8], String> {
    map(take(3usize), |s: &[u8]| String::from_utf8_lossy(s).into_owned())(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], String> {
    map(take(3usize), |s: &[u8]| String::from_utf8_lossy(s).into_owned())(input)
}

fn parse_packed_fields(input: &[u8]) -> IResult<&[u8], PackedFields> {
    map(
        be_u8,
        |pf: u8| PackedFields {
            global_color_table_size: pf & 0x07,
            sort_flag: (pf & 0x08) != 0,
            color_resolution: (pf & 0x70) >> 4,
            global_color_table_flag: (pf & 0x80) != 0,
        },
    )(input)
}

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<Color>> {
    let (input, colors) = take(size)(input)?;
    let colors: Vec<Color> = colors
        .chunks(3)
        .map(|c| Color {
            red: c[0],
            green: c[1],
            blue: c[2],
        })
        .collect();
    Ok((input, colors))
}

fn parse_image_separator(input: &[u8]) -> IResult<&[u8], u8> {
    be_u8(input)
}

fn parse_image(input: &[u8]) -> IResult<&[u8], Image> {
    let (input, (image_separator, image_left_position, image_top_position, image_width, image_height, packed_fields)) =
        tuple((
            parse_image_separator,
            be_u16,
            be_u16,
            be_u16,
            be_u16,
            parse_packed_fields,
        ))(input)?;
    let (input, local_color_table) = if packed_fields.global_color_table_flag {
        let size = 3 * (1 << (packed_fields.global_color_table_size + 1));
        let (input, color_table) = parse_color_table(input, size)?;
        (input, Some(color_table))
    } else {
        (input, None)
    };
    let (input, image_data) = take_while(|i| *i != 0x3b)(input)?;
    Ok((
        input,
        Image {
            image_separator,
            image_left_position,
            image_top_position,
            image_width,
            image_height,
            packed_fields,
            local_color_table,
            image_data: image_data.to_vec(),
        },
    ))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, (signature, version, logical_screen_width, logical_screen_height, packed_fields, background_color_index, pixel_aspect_ratio)) =
        tuple((
            parse_signature,
            parse_version,
            be_u16,
            be_u16,
            parse_packed_fields,
            be_u8,
            be_u8,
        ))(input)?;
    let (input, global_color_table) = if packed_fields.global_color_table_flag {
        let size = 3 * (1 << (packed_fields.global_color_table_size + 1));
        let (input, color_table) = parse_color_table(input, size)?;
        (input, Some(color_table))
    } else {
        (input, None)
    };
    let mut images = Vec::new();
    let mut input = input;
    while input.len() > 0 && input[0] == 0x2c {
        let (input_rest, image) = parse_image(input)?;
        images.push(image);
        input = input_rest;
    }
    Ok((
        input,
        Gif {
            signature,
            version,
            logical_screen_width,
            logical_screen_height,
            packed_fields,
            background_color_index,
            pixel_aspect_ratio,
            global_color_table,
            images,
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }
    let mut file = File::open(&args[1])?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    let result = parse_gif(&data);
    match result {
        Ok((_, gif)) => println!("{:?}", gif),
        Err(err) => println!("Error parsing GIF: {:?}", err),
    }
    Ok(())
}