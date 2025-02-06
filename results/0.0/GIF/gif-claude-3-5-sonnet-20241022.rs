use nom::{
    bytes::complete::{tag, take},
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
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_field: u8,
}

#[derive(Debug)]
struct GraphicsControlExtension {
    block_size: u8,
    packed_field: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct ApplicationExtension {
    block_size: u8,
    identifier: [u8; 8],
    auth_code: [u8; 3],
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
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
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

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<RGB>> {
    let mut colors = Vec::new();
    let mut remaining = input;

    for _ in 0..size {
        let (input, (r, g, b)) = tuple((le_u8, le_u8, le_u8))(remaining)?;
        colors.push(RGB { r, g, b });
        remaining = input;
    }

    Ok((remaining, colors))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag([0x2C])(input)?;
    let (input, (left, top, width, height, packed_field)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
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

fn parse_graphics_control_extension(input: &[u8]) -> IResult<&[u8], GraphicsControlExtension> {
    let (input, _) = tag([0x21, 0xF9])(input)?;
    let (input, (block_size, packed_field, delay_time, transparent_color_index, _)) =
        tuple((le_u8, le_u8, le_u16, le_u8, tag([0x00])))(input)?;
    Ok((
        input,
        GraphicsControlExtension {
            block_size,
            packed_field,
            delay_time,
            transparent_color_index,
        },
    ))
}

fn parse_application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, _) = tag([0x21, 0xFF])(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, identifier) = take(8usize)(input)?;
    let (input, auth_code) = take(3usize)(input)?;
    Ok((
        input,
        ApplicationExtension {
            block_size,
            identifier: identifier.try_into().unwrap(),
            auth_code: auth_code.try_into().unwrap(),
        },
    ))
}

fn parse_sub_blocks(input: &[u8]) -> IResult<&[u8], Vec<Vec<u8>>> {
    let mut blocks = Vec::new();
    let mut remaining = input;

    loop {
        let (input, block_size) = le_u8(remaining)?;
        if block_size == 0 {
            return Ok((input, blocks));
        }
        let (input, block_data) = take(block_size as usize)(input)?;
        blocks.push(block_data.to_vec());
        remaining = input;
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gif_file>", args[0]);
        return;
    }

    let mut file = File::open(&args[1]).expect("Failed to open file");
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).expect("Failed to read file");

    let (input, header) = parse_header(&buffer).expect("Failed to parse header");
    println!("Header: {:?}", header);

    let (input, lsd) = parse_logical_screen_descriptor(input).expect("Failed to parse LSD");
    println!("Logical Screen Descriptor: {:?}", lsd);

    let mut current_input = input;
    while !current_input.is_empty() {
        match current_input[0] {
            0x2C => {
                let (input, descriptor) =
                    parse_image_descriptor(current_input).expect("Failed to parse image descriptor");
                println!("Image Descriptor: {:?}", descriptor);
                current_input = input;
            }
            0x21 => {
                match current_input[1] {
                    0xF9 => {
                        let (input, gce) = parse_graphics_control_extension(current_input)
                            .expect("Failed to parse graphics control extension");
                        println!("Graphics Control Extension: {:?}", gce);
                        current_input = input;
                    }
                    0xFF => {
                        let (input, app_ext) = parse_application_extension(current_input)
                            .expect("Failed to parse application extension");
                        println!("Application Extension: {:?}", app_ext);
                        let (input, blocks) =
                            parse_sub_blocks(input).expect("Failed to parse sub-blocks");
                        println!("Application Data Blocks: {} blocks", blocks.len());
                        current_input = input;
                    }
                    _ => {
                        let (input, blocks) =
                            parse_sub_blocks(&current_input[2..]).expect("Failed to parse sub-blocks");
                        println!("Extension Blocks: {} blocks", blocks.len());
                        current_input = input;
                    }
                }
            }
            0x3B => break, // Trailer
            _ => {
                println!("Unknown block type: {:02X}", current_input[0]);
                break;
            }
        }
    }
}