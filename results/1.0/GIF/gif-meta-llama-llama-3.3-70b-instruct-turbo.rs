use nom::{
    bytes::complete::{take, take_while_m_n},
    combinator::{map, map_opt, map_res},
    error::{context, ErrorKind},
    multi::{length_data, many_till, separated_pair},
    number::complete::{be_u16, be_u32},
    sequence::{delimited, pair, tuple},
    IResult,
};
use std::{env, fs, io, str};

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 3],
    version: [u8; 3],
}

impl GifHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], GifHeader> {
        context(
            "GifHeader",
            map(
                tuple((take(3u8), take(3u8))),
                |(signature, version)| GifHeader { signature, version },
            ),
        )(input)
    }
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    flags: u8,
    background_color_index: u8,
    aspect_ratio: u8,
}

impl LogicalScreenDescriptor {
    fn parse(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
        context(
            "LogicalScreenDescriptor",
            map(
                tuple((be_u16, be_u16, take(1u8), take(1u8), take(1u8))),
                |(width, height, flags, background_color_index, aspect_ratio)| {
                    LogicalScreenDescriptor {
                        width,
                        height,
                        flags,
                        background_color_index,
                        aspect_ratio,
                    }
                },
            ),
        )(input)
    }
}

#[derive(Debug)]
struct ColorTable {
    colors: Vec<[u8; 3]>,
}

impl ColorTable {
    fn parse(input: &[u8], size: usize) -> IResult<&[u8], ColorTable> {
        context(
            "ColorTable",
            map(
                take(size * 3),
                |colors: &[u8]| {
                    let mut result = Vec::new();
                    for chunk in colors.chunks(3) {
                        let mut color = [0; 3];
                        color.copy_from_slice(chunk);
                        result.push(color);
                    }
                    ColorTable { colors: result }
                },
            ),
        )(input)
    }
}

#[derive(Debug)]
enum ImageDescriptorType {
    Separator,
    Image,
    Extension,
}

impl ImageDescriptorType {
    fn parse(input: &[u8]) -> IResult<&[u8], ImageDescriptorType> {
        context(
            "ImageDescriptorType",
            map_opt(take(1u8), |input: &[u8]| {
                match input[0] {
                    0x2c => Some(ImageDescriptorType::Image),
                    0x21 => Some(ImageDescriptorType::Extension),
                    0x3b => Some(ImageDescriptorType::Separator),
                    _ => None,
                }
            }),
        )(input)
    }
}

#[derive(Debug)]
struct ImageDescriptor {
    image_type: ImageDescriptorType,
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    flags: u8,
}

impl ImageDescriptor {
    fn parse(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
        context(
            "ImageDescriptor",
            map(
                tuple((
                    ImageDescriptorType::parse,
                    be_u16,
                    be_u16,
                    be_u16,
                    be_u16,
                    take(1u8),
                )),
                |(image_type, left, top, width, height, flags)| ImageDescriptor {
                    image_type,
                    left,
                    top,
                    width,
                    height,
                    flags,
                },
            ),
        )(input)
    }
}

#[derive(Debug)]
enum BlockType {
    PlainText,
    Application,
    Comment,
}

impl BlockType {
    fn parse(input: &[u8]) -> IResult<&[u8], BlockType> {
        context(
            "BlockType",
            map_opt(take(1u8), |input: &[u8]| {
                match input[0] {
                    0x01 => Some(BlockType::PlainText),
                    0xfe => Some(BlockType::Comment),
                    0xff => Some(BlockType::Application),
                    _ => None,
                }
            }),
        )(input)
    }
}

#[derive(Debug)]
struct ExtensionBlock {
    block_type: BlockType,
    data: Vec<u8>,
}

impl ExtensionBlock {
    fn parse(input: &[u8]) -> IResult<&[u8], ExtensionBlock> {
        context(
            "ExtensionBlock",
            map(
                pair(
                    BlockType::parse,
                    length_data(take_while_m_n(1, 255, |c| c != 0x00)),
                ),
                |(block_type, data)| ExtensionBlock { block_type, data },
            ),
        )(input)
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], ()> {
    context(
        "Gif",
        map(
            tuple((
                GifHeader::parse,
                LogicalScreenDescriptor::parse,
                length_data(ColorTable::parse),
                many_till(
                    separated_pair(
                        ImageDescriptor::parse,
                        take_while_m_n(1, 255, |c| c != 0x00),
                        length_data(take(1)),
                    ),
                    take(1u8),
                ),
            )),
            |_| (),
        ),
    )(input)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: gif_parser <input_file>",
        ));
    }
    let input = fs::read(args[1].clone())?;
    match parse_gif(&input) {
        Ok((remaining, _)) => {
            if remaining.len() > 0 {
                eprintln!("Warning: {} bytes of input remaining", remaining.len());
            }
        }
        Err(err) => {
            eprintln!("Error: {}", err);
            return Err(io::Error::new(io::ErrorKind::InvalidInput, "Invalid GIF"));
        }
    }
    Ok(())
}