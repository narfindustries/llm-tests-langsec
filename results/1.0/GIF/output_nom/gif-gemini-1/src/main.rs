use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    number::complete::le_u16,
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 6],
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<[u8; 3]>>,
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
struct ImageDescriptor {
    image_separator: u8,
    image_left: u16,
    image_top: u16,
    image_width: u16,
    image_height: u16,
    packed_fields: u8,
    local_color_table: Option<Vec<[u8; 3]>>,
    image_data: Vec<u8>,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = tag("GIF89a")(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let (input, global_color_table) = parse_global_color_table(input, logical_screen_descriptor.packed_fields)?;
    Ok((
        input,
        GifHeader {
            signature: signature.try_into().unwrap(),
            logical_screen_descriptor,
            global_color_table,
        },
    ))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) = tuple((
        le_u16,
        le_u16,
        take(1u8),
        take(1u8),
        take(1u8),
    ))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            packed_fields: packed_fields[0],
            background_color_index: background_color_index[0],
            pixel_aspect_ratio: pixel_aspect_ratio[0],
        },
    ))
}

fn parse_global_color_table(input: &[u8], packed_fields: u8) -> IResult<&[u8], Option<Vec<[u8; 3]>>> {
    let global_color_table_flag = (packed_fields >> 7) & 1;
    if global_color_table_flag == 1 {
        let color_table_size = ((packed_fields >> 4) & 7) as usize;
        let table_size = (1usize << (color_table_size + 1)) * 3;
        let (input, color_table) = take(table_size)(input)?;
        let mut color_table_vec = Vec::new();
        for i in 0..(1usize << (color_table_size + 1)) {
            let color = [color_table[i * 3], color_table[i * 3 + 1], color_table[i * 3 + 2]];
            color_table_vec.push(color);
        }
        Ok((input, Some(color_table_vec)))
    } else {
        Ok((input, None))
    }
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, image_separator) = tag(&[0x2C])(input)?;
    let (input, (image_left, image_top, image_width, image_height, packed_fields)) = tuple((
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        take(1u8),
    ))(input)?;
    let (input, local_color_table) = parse_local_color_table(input, packed_fields[0])?;
    let (input, image_data) = parse_image_data(input)?;
    Ok((
        input,
        ImageDescriptor {
            image_separator: image_separator[0],
            image_left,
            image_top,
            image_width,
            image_height,
            packed_fields: packed_fields[0],
            local_color_table,
            image_data,
        },
    ))
}

fn parse_local_color_table(input: &[u8], packed_fields: u8) -> IResult<&[u8], Option<Vec<[u8; 3]>>> {
    let local_color_table_flag = (packed_fields >> 7) & 1;
    if local_color_table_flag == 1 {
        let color_table_size = ((packed_fields >> 4) & 7) as usize;
        let table_size = (1usize << (color_table_size + 1)) * 3;
        let (input, color_table) = take(table_size)(input)?;
        let mut color_table_vec = Vec::new();
        for i in 0..(1usize << (color_table_size + 1)) {
            let color = [color_table[i * 3], color_table[i * 3 + 1], color_table[i * 3 + 2]];
            color_table_vec.push(color);
        }
        Ok((input, Some(color_table_vec)))
    } else {
        Ok((input, None))
    }
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut data = Vec::new();
    let mut i = 0;
    loop {
        let block_size = input[i];
        if block_size == 0 {
            break;
        }
        data.extend_from_slice(&input[i + 1..i + 1 + block_size as usize]);
        i += 1 + block_size as usize;
    }
    Ok((&input[i + 1..], data))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], (GifHeader, Vec<ImageDescriptor>)> {
    let (input, header) = parse_gif_header(input)?;
    let mut images = Vec::new();
    let mut remaining_input = input;
    loop {
        match parse_image_descriptor(remaining_input) {
            Ok((rest, image)) => {
                images.push(image);
                remaining_input = rest;
            }
            Err(_) => break,
        }
    }
    let (input, _) = tag(&[0x3B])(remaining_input)?;
    Ok((input, (header, images)))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: gif_parser <filename>");
        return;
    }

    let filename = &args[1];
    let mut file = match File::open(filename) {
        Ok(file) => file,
        Err(err) => {
            println!("Error opening file: {}", err);
            return;
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            println!("Error reading file: {}", err);
            return;
        }
    };

    match parse_gif(&buffer) {
        Ok((_, (header, images))) => {
            println!("GIF Header: {:?}", header);
            for image in images {
                println!("Image Descriptor: {:?}", image);
            }
        }
        Err(nom::Err::Error(e) | nom::Err::Failure(e)) => {
            println!("Error parsing GIF: {:?}", e);
        }
        Err(nom::Err::Incomplete(_)) => {
            println!("Incomplete GIF data");
        }
    }
}
