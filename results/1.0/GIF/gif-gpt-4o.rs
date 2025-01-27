use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    IResult,
    number::complete::{le_u8, le_u16},
    sequence::{tuple, preceded, terminated},
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::many0,
};

#[derive(Debug)]
pub struct GifHeader {
    signature: String,
    version: String,
}

impl GifHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, (signature, version)) = tuple((take(3usize), take(3usize)))(input)?;
        Ok((
            input,
            GifHeader {
                signature: String::from_utf8_lossy(signature).to_string(),
                version: String::from_utf8_lossy(version).to_string(),
            }
        ))
    }
}

#[derive(Debug)]
pub struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    fields: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

impl LogicalScreenDescriptor {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, (width, height, fields, background_color_index, pixel_aspect_ratio)) =
            tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
        Ok((
            input,
            LogicalScreenDescriptor {
                width,
                height,
                fields,
                background_color_index,
                pixel_aspect_ratio,
            }
        ))
    }
}

#[derive(Debug)]
pub struct ColorTable(Vec<u8>);

impl ColorTable {
    fn parse(size: usize) -> impl Fn(&[u8]) -> IResult<&[u8], Self> {
        move |input| {
            let (input, data) = take(size)(input)?;
            Ok((input, ColorTable(data.to_vec())))
        }
    }
}

#[derive(Debug)]
pub struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    fields: u8,
}

impl ImageDescriptor {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, (left, top, width, height, fields)) = tuple((le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
        Ok((
            input,
            ImageDescriptor {
                left,
                top,
                width,
                height,
                fields,
            }
        ))
    }
}

#[derive(Debug)]
pub struct DataSubBlock {
    size: u8,
    data: Vec<u8>,
}

impl DataSubBlock {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, size) = le_u8(input)?;
        let (input, data) = take(size)(input)?;
        Ok((input, DataSubBlock { size, data: data.to_vec() }))
    }
}

#[derive(Debug)]
pub struct ImageData {
    lzw_min_code_size: u8,
    data_blocks: Vec<DataSubBlock>,
}

impl ImageData {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, lzw_min_code_size) = le_u8(input)?;
        let (input, data_blocks) = many0(DataSubBlock::parse)(input)?;
        Ok((
            input,
            ImageData {
                lzw_min_code_size,
                data_blocks,
            }
        ))
    }
}

#[derive(Debug)]
pub struct GraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

impl GraphicControlExtension {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, (_, packed_fields, delay_time, transparent_color_index, _)) =
            tuple((tag(&[0x04]), le_u8, le_u16, le_u8, tag(&[0x00])))(input)?;
        Ok((
            input,
            GraphicControlExtension {
                packed_fields,
                delay_time,
                transparent_color_index,
            }
        ))
    }
}

#[derive(Debug)]
pub struct GifFile {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<ColorTable>,
    blocks: Vec<BlockType>,
}

impl GifFile {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, header) = GifHeader::parse(input)?;
        let (input, logical_screen_descriptor) = LogicalScreenDescriptor::parse(input)?;

        let global_color_table_flag = (logical_screen_descriptor.fields & 0b1000_0000) != 0;
        let global_color_table_size = if global_color_table_flag {
            3 * (2usize.pow((logical_screen_descriptor.fields & 0b0000_0111u8).into()) + 1)
        } else {
            0
        };

        let (input, global_color_table) = if global_color_table_flag {
            map(ColorTable::parse(global_color_table_size), Some)(input)?
        } else {
            (input, None)
        };

        let mut blocks = Vec::new();
        let mut input = input;

        while !input.is_empty() {
            let (new_input, block) = match input[0] {
                0x21 if input[1] == 0xF9 => {
                    let (input, _) = tag(&[0x21, 0xF9])(input)?;
                    let (input, extension) = GraphicControlExtension::parse(input)?;
                    (input, BlockType::GraphicControlExtension(extension))
                }
                0x2C => {
                    let (input, _) = tag(&[0x2C])(input)?;
                    let (input, descriptor) = ImageDescriptor::parse(input)?;
                    let local_color_table_flag = descriptor.fields & 0b1000_0000 != 0;
                    let local_color_table_size = if local_color_table_flag {
                        3 * (2usize.pow((descriptor.fields & 0b0000_0111u8).into()) + 1)
                    } else {
                        0
                    };

                    let (input, local_color_table) = if local_color_table_flag {
                        map(ColorTable::parse(local_color_table_size), Some)(input)?
                    } else {
                        (input, None)
                    };

                    let (input, image_data) = ImageData::parse(input)?;
                    (input, BlockType::Image { descriptor, local_color_table, image_data })
                }
                0x3B => break,
                _ => return Err(nom::Err::Error((input, nom::error::ErrorKind::Tag))),
            };

            blocks.push(block);
            input = new_input;
        }

        Ok((
            input,
            GifFile {
                header,
                logical_screen_descriptor,
                global_color_table,
                blocks,
            }
        ))
    }
}

#[derive(Debug)]
pub enum BlockType {
    GraphicControlExtension(GraphicControlExtension),
    Image {
        descriptor: ImageDescriptor,
        local_color_table: Option<ColorTable>,
        image_data: ImageData,
    },
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match GifFile::parse(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }

    Ok(())
}