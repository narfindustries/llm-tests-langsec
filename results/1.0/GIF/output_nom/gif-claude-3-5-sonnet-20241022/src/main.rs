use nom::{
    bytes::complete::{tag, take},
    combinator::map,
    multi::{count, many0},
    number::complete::{le_u16, le_u8, u8},
    sequence::{pair, preceded, tuple},
    IResult,
};
use std::{env, fs, process};

#[derive(Debug)]
struct GIF {
    header: Header,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<RGB>>,
    blocks: Vec<Block>,
    trailer: u8,
}

#[derive(Debug)]
struct Header {
    signature: [u8; 3],
    version: [u8; 3],
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed: u8,
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
enum Block {
    GraphicControl {
        byte_size: u8,
        packed: u8,
        delay_time: u16,
        transparent_color_index: u8,
        terminator: u8,
    },
    Image {
        left: u16,
        top: u16,
        width: u16,
        height: u16,
        packed: u8,
        local_color_table: Option<Vec<RGB>>,
        image_data: ImageData,
    },
    Extension {
        label: u8,
        data: Vec<DataSubBlock>,
    },
    Comment(Vec<DataSubBlock>),
    PlainText {
        byte_size: u8,
        left: u16,
        top: u16,
        width: u16,
        height: u16,
        cell_width: u8,
        cell_height: u8,
        foreground_color: u8,
        background_color: u8,
        data: Vec<DataSubBlock>,
    },
    ApplicationExtension {
        byte_size: u8,
        identifier: [u8; 8],
        auth_code: [u8; 3],
        data: Vec<DataSubBlock>,
    },
}

#[derive(Debug)]
struct ImageData {
    lzw_minimum_code_size: u8,
    data: Vec<DataSubBlock>,
}

#[derive(Debug)]
struct DataSubBlock {
    size: u8,
    data: Vec<u8>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], Header> {
    map(
        tuple((take(3usize), take(3usize))),
        |(signature, version)| Header {
            signature: signature.try_into().unwrap(),
            version: version.try_into().unwrap(),
        },
    )(input)
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    map(
        tuple((le_u16, le_u16, u8, u8, u8)),
        |(width, height, packed, background_color_index, pixel_aspect_ratio)| LogicalScreenDescriptor {
            width,
            height,
            packed,
            background_color_index,
            pixel_aspect_ratio,
        },
    )(input)
}

fn parse_rgb(input: &[u8]) -> IResult<&[u8], RGB> {
    map(tuple((u8, u8, u8)), |(r, g, b)| RGB { r, g, b })(input)
}

fn parse_data_sub_block(input: &[u8]) -> IResult<&[u8], DataSubBlock> {
    let (input, size) = u8(input)?;
    let (input, data) = take(size)(input)?;
    Ok((
        input,
        DataSubBlock {
            size,
            data: data.to_vec(),
        },
    ))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], ImageData> {
    let (input, lzw_minimum_code_size) = u8(input)?;
    let (input, data) = many0(parse_data_sub_block)(input)?;
    let (input, _) = tag(&[0])(input)?;
    Ok((
        input,
        ImageData {
            lzw_minimum_code_size,
            data,
        },
    ))
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, block_type) = u8(input)?;
    match block_type {
        0x21 => {
            let (input, extension_type) = u8(input)?;
            match extension_type {
                0xF9 => {
                    let (input, byte_size) = u8(input)?;
                    let (input, (packed, delay_time, transparent_color_index, terminator)) =
                        tuple((u8, le_u16, u8, u8))(input)?;
                    Ok((
                        input,
                        Block::GraphicControl {
                            byte_size,
                            packed,
                            delay_time,
                            transparent_color_index,
                            terminator,
                        },
                    ))
                }
                0xFE => {
                    let (input, data) = many0(parse_data_sub_block)(input)?;
                    let (input, _) = tag(&[0])(input)?;
                    Ok((input, Block::Comment(data)))
                }
                0x01 => {
                    let (input, byte_size) = u8(input)?;
                    let (input, (left, top, width, height, cell_width, cell_height, foreground_color, background_color)) =
                        tuple((le_u16, le_u16, le_u16, le_u16, u8, u8, u8, u8))(input)?;
                    let (input, data) = many0(parse_data_sub_block)(input)?;
                    let (input, _) = tag(&[0])(input)?;
                    Ok((
                        input,
                        Block::PlainText {
                            byte_size,
                            left,
                            top,
                            width,
                            height,
                            cell_width,
                            cell_height,
                            foreground_color,
                            background_color,
                            data,
                        },
                    ))
                }
                0xFF => {
                    let (input, byte_size) = u8(input)?;
                    let (input, identifier) = take(8usize)(input)?;
                    let (input, auth_code) = take(3usize)(input)?;
                    let (input, data) = many0(parse_data_sub_block)(input)?;
                    let (input, _) = tag(&[0])(input)?;
                    Ok((
                        input,
                        Block::ApplicationExtension {
                            byte_size,
                            identifier: identifier.try_into().unwrap(),
                            auth_code: auth_code.try_into().unwrap(),
                            data,
                        },
                    ))
                }
                _ => {
                    let (input, data) = many0(parse_data_sub_block)(input)?;
                    let (input, _) = tag(&[0])(input)?;
                    Ok((
                        input,
                        Block::Extension {
                            label: extension_type,
                            data,
                        },
                    ))
                }
            }
        }
        0x2C => {
            let (input, (left, top, width, height, packed)) =
                tuple((le_u16, le_u16, le_u16, le_u16, u8))(input)?;
            let has_local_color_table = (packed & 0x80) != 0;
            let color_table_size = if has_local_color_table {
                1 << ((packed & 0x07) + 1)
            } else {
                0
            };
            let (input, local_color_table) = if has_local_color_table {
                let (input, table) = count(parse_rgb, color_table_size)(input)?;
                (input, Some(table))
            } else {
                (input, None)
            };
            let (input, image_data) = parse_image_data(input)?;
            Ok((
                input,
                Block::Image {
                    left,
                    top,
                    width,
                    height,
                    packed,
                    local_color_table,
                    image_data,
                },
            ))
        }
        _ => panic!("Unknown block type: {}", block_type),
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GIF> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let has_global_color_table = (logical_screen_descriptor.packed & 0x80) != 0;
    let color_table_size = if has_global_color_table {
        1 << ((logical_screen_descriptor.packed & 0x07) + 1)
    } else {
        0
    };
    let (input, global_color_table) = if has_global_color_table {
        let (input, table) = count(parse_rgb, color_table_size)(input)?;
        (input, Some(table))
    } else {
        (input, None)
    };
    let (input, blocks) = many0(parse_block)(input)?;
    let (input, trailer) = tag(&[0x3B])(input)?;
    Ok((
        input,
        GIF {
            header,
            logical_screen_descriptor,
            global_color_table,
            blocks,
            trailer: trailer[0],
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gif-file>", args[0]);
        process::exit(1);
    }

    let data = fs::read(&args[1]).expect("Failed to read file");
    match parse_gif(&data) {
        Ok((remaining, gif)) => {
            if !remaining.is_empty() {
                eprintln!("Warning: {} unparsed bytes remaining", remaining.len());
            }
            println!("{:#?}", gif);
        }
        Err(e) => {
            eprintln!("Failed to parse GIF: {}", e);
            process::exit(1);
        }
    }
}