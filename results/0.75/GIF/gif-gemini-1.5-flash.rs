use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, value},
    error::{context, VerboseError},
    multi::count,
    number::complete::le_u16,
    sequence::{preceded, tuple},
    IResult,
    bytes::complete::rest,
};
use std::fs;
use std::path::Path;


#[derive(Debug)]
struct Gif {
    signature: [u8; 6],
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<[u8; 3]>>,
    images: Vec<Image>,
    trailer: u8,
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    screen_width: u16,
    screen_height: u16,
    packed_fields: u8,
    backgroundColorIndex: u8,
    pixelAspectRatio: u8,
}

#[derive(Debug)]
struct Image {
    image_separator: [u8; 1],
    image_left: u16,
    image_top: u16,
    image_width: u16,
    image_height: u16,
    packed_fields: u8,
    local_color_table: Option<Vec<[u8; 3]>>,
    lzw_minimum_code_size: u8,
    imageData: Vec<u8>,
}


fn gif(input: &[u8]) -> IResult<&[u8], Gif, VerboseError<&[u8]>> {
    let (input, signature) = tag("GIF89a")(input)?;
    let (input, logical_screen_descriptor) = context("logical_screen_descriptor",logical_screen_descriptor)(input)?;
    let (input, global_color_table) = opt(global_color_table)(input)?;
    let (input, images) = many0(image)(input)?;
    let (input, trailer) = tag(";")(input)?;
    Ok((
        input,
        Gif {
            signature,
            logical_screen_descriptor,
            global_color_table,
            images,
            trailer: trailer[0],
        },
    ))
}

fn logical_screen_descriptor(
    input: &[u8],
) -> IResult<&[u8], LogicalScreenDescriptor, VerboseError<&[u8]>> {
    let (input, (screen_width, screen_height, packed_fields)) = tuple((
        le_u16,
        le_u16,
        take(1usize),
    ))(input)?;
    let (input, backgroundColorIndex) = take(1usize)(input)?;
    let (input, pixelAspectRatio) = take(1usize)(input)?;

    Ok((
        input,
        LogicalScreenDescriptor {
            screen_width,
            screen_height,
            packed_fields: packed_fields[0],
            backgroundColorIndex: backgroundColorIndex[0],
            pixelAspectRatio: pixelAspectRatio[0],
        },
    ))
}

fn global_color_table(input: &[u8]) -> IResult<&[u8], Vec<[u8; 3]>, VerboseError<&[u8]>> {
    let (input, size) = take(1usize)(input)?;
    let size = (size[0] as usize) & 0b00000011;
    let num_colors = 1usize << (size + 1);
    let (input, color_table) = count(take(3usize), num_colors)(input)?;
    Ok((input, color_table.iter().map(|x| [x[0], x[1], x[2]]).collect()))
}

fn image(input: &[u8]) -> IResult<&[u8], Image, VerboseError<&[u8]>> {
    let (input, image_separator) = tag(",")(input)?;
    let (input, (image_left, image_top, image_width, image_height, packed_fields)) = tuple((
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        take(1usize),
    ))(input)?;
    let (input, local_color_table) = opt(global_color_table)(input)?;
    let (input, lzw_minimum_code_size) = take(1usize)(input)?;
    let (input, imageData) = rest(input)?;
    Ok((
        input,
        Image {
            image_separator: [image_separator[0]],
            image_left,
            image_top,
            image_width,
            image_height,
            packed_fields: packed_fields[0],
            local_color_table,
            lzw_minimum_code_size: lzw_minimum_code_size[0],
            imageData: imageData.to_vec(),
        },
    ))
}

fn many0<F, O, E>(f: F) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<O>, E>
where
    F: for<'a> Fn(&'a [u8]) -> IResult<&'a [u8], O, E>,
    E: nom::error::ParseError<&[u8]> + nom::error::ContextError<&[u8]>,
{
    nom::multi::many0(f)
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: gif_parser <filename>");
        return;
    }

    let filename = &args[1];
    let path = Path::new(filename);
    let data = match fs::read(path) {
        Ok(data) => data,
        Err(e) => {
            println!("Error reading file: {}", e);
            return;
        }
    };

    match gif(&data) {
        Ok((remaining, gif)) => {
            println!("GIF parsed successfully!\n{:?}", gif);
            if !remaining.is_empty() {
                println!("Remaining data: {:?}", remaining);
            }
        }
        Err(e) => {
            println!("Error parsing GIF: {:?}", e);
        }
    }
}