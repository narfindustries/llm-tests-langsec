use nom::{
    bytes::complete::{tag, take},
    combinator::{map},
    multi::{many1},
    number::complete::{be_u16, be_u8},
    IResult,
    error::{ErrorKind, ParseError},
};
use std::env;
use std::fs::File;
use std::io::{Read, stdin};

#[derive(Debug, PartialEq)]
enum ColorTable {
    Global(Vec<(u8, u8, u8)>),
    Local(Vec<(u8, u8, u8)>),
}

#[derive(Debug, PartialEq)]
struct ImageDescriptor {
    image_separator: u8,
    image_left: u16,
    image_top: u16,
    image_width: u16,
    image_height: u16,
    local_color_table_flag: bool,
    interlace_flag: bool,
    local_color_table: Option<ColorTable>,
}

#[derive(Debug, PartialEq)]
struct Gif {
    signature: String,
    version: String,
    logical_screen_width: u16,
    logical_screen_height: u16,
    packed_fields: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
    global_color_table: Option<ColorTable>,
    image_descriptors: Vec<ImageDescriptor>,
}

fn parse_signature(input: &[u8]) -> IResult<&[u8], String> {
    map(take(3usize), |s: &[u8]| String::from_utf8_lossy(s).into_owned())(input)
}

fn parse_version(input: &[u8]) -> IResult<&[u8], String> {
    map(take(3usize), |s: &[u8]| String::from_utf8_lossy(s).into_owned())(input)
}

fn parse_packed_fields(input: &[u8]) -> IResult<&[u8], u8> {
    map(be_u8, |x: u8| x)(input)
}

fn parse_global_color_table(input: &[u8], size: u8) -> IResult<&[u8], Vec<(u8, u8, u8)>> {
    let count = 2usize.pow(size as u32 + 1);
    let colors: IResult<&[u8], Vec<(u8, u8, u8)>> = many1(map(take(3usize), |s: &[u8]| (s[0], s[1], s[2])))(input);
    let (input, colors) = colors?;
    if colors.len() != count {
        return Err(ErrorKind::Count.into_error(input));
    }
    Ok((input, colors))
}

fn parse_local_color_table(input: &[u8], size: u8) -> IResult<&[u8], Vec<(u8, u8, u8)>> {
    parse_global_color_table(input, size)
}

fn parse_image_separator(input: &[u8]) -> IResult<&[u8], u8> {
    map(tag([0x2c]), |_: &[u8]| 0x2c)(input)
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, image_separator) = parse_image_separator(input)?;
    let (input, image_left) = be_u16(input)?;
    let (input, image_top) = be_u16(input)?;
    let (input, image_width) = be_u16(input)?;
    let (input, image_height) = be_u16(input)?;
    let (input, packed_fields) = be_u8(input)?;
    let local_color_table_flag = packed_fields & 0x80 != 0;
    let interlace_flag = packed_fields & 0x40 != 0;
    let local_color_table_size = (packed_fields & 0x07) + 1;
    let (input, local_color_table) = if local_color_table_flag {
        let (input, local_color_table) = parse_local_color_table(input, local_color_table_size)?;
        (input, Some(ColorTable::Local(local_color_table)))
    } else {
        (input, None)
    };
    Ok((
        input,
        ImageDescriptor {
            image_separator,
            image_left,
            image_top,
            image_width,
            image_height,
            local_color_table_flag,
            interlace_flag,
            local_color_table,
        },
    ))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, signature) = parse_signature(input)?;
    let (input, version) = parse_version(input)?;
    let (input, logical_screen_width) = be_u16(input)?;
    let (input, logical_screen_height) = be_u16(input)?;
    let (input, packed_fields) = parse_packed_fields(input)?;
    let global_color_table_flag = packed_fields & 0x80 != 0;
    let (input, background_color_index) = be_u8(input)?;
    let (input, pixel_aspect_ratio) = be_u8(input)?;
    let global_color_table_size = (packed_fields & 0x07) + 1;
    let (input, global_color_table) = if global_color_table_flag {
        let (input, global_color_table) = parse_global_color_table(input, global_color_table_size)?;
        (input, Some(ColorTable::Global(global_color_table)))
    } else {
        (input, None)
    };
    let (input, image_descriptors) = many1(parse_image_descriptor)(input)?;
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
            image_descriptors,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut file: Box<dyn Read> = if args.len() > 1 {
        Box::new(File::open(&args[1]).expect("Failed to open file"))
    } else {
        Box::new(stdin())
    };
    let mut data = Vec::new();
    file.read_to_end(&mut data).expect("Failed to read file");
    let result = parse_gif(&data);
    match result {
        Ok((_, gif)) => println!("{:?}", gif),
        Err(err) => println!("Error: {:?}", err),
    }
}