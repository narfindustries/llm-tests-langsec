use std::env;
use std::fs::File;
use std::io::Read;
use nom::bytes::complete::{take, tag};
use nom::combinator::{map, opt};
use nom::multi::{length_data, many_till};
use nom::number::complete::{be_u16, be_u8};
use nom::sequence::{tuple, preceded};
use nom::IResult;

#[derive(Debug, PartialEq)]
enum ColorTable {
    rgb(u8, u8, u8),
}

impl ColorTable {
    fn rgb(r: u8, g: u8, b: u8) -> Self {
        ColorTable::rgb(r, g, b)
    }
}

#[derive(Debug, PartialEq)]
struct GifHeader {
    signature: [u8; 3],
    version: [u8; 3],
}

impl GifHeader {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, signature) = take(3u8)(input)?;
        let (input, version) = take(3u8)(input)?;
        Ok((input, GifHeader { signature, version }))
    }
}

#[derive(Debug, PartialEq)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

impl LogicalScreenDescriptor {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, width) = be_u16(input)?;
        let (input, height) = be_u16(input)?;
        let (input, packed_fields) = be_u8(input)?;
        let (input, background_color_index) = be_u8(input)?;
        let (input, pixel_aspect_ratio) = be_u8(input)?;
        Ok((input, LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
            background_color_index,
            pixel_aspect_ratio,
        }))
    }
}

#[derive(Debug, PartialEq)]
struct GlobalColorTable {
    colors: Vec<ColorTable>,
}

impl GlobalColorTable {
    fn parse(input: &[u8], size: usize) -> IResult<&[u8], Self> {
        let (input, colors) = length_data(take(size * 3))(input)?;
        let colors: Vec<ColorTable> = colors
            .chunks(3)
            .map(|c| ColorTable::rgb(c[0], c[1], c[2]))
            .collect();
        Ok((input, GlobalColorTable { colors }))
    }
}

#[derive(Debug, PartialEq)]
enum Block {
    ImageDescriptor(u8, u8, u8, u8),
    Extension(u8, Vec<u8>),
    Terminator,
}

impl Block {
    fn parse(input: &[u8]) -> IResult<&[u8], Self> {
        let (input, block_type) = be_u8(input)?;
        match block_type {
            0x2c => {
                let (input, left) = be_u8(input)?;
                let (input, top) = be_u8(input)?;
                let (input, width) = be_u8(input)?;
                let (input, height) = be_u8(input)?;
                Ok((input, Block::ImageDescriptor(left, top, width, height)))
            }
            0x21 => {
                let (input, extension_label) = be_u8(input)?;
                let (input, data: Vec<_>) = length_data(many_till(take(1u8), tag(&[0x00])))(input)?;
                Ok((input, Block::Extension(extension_label, data)))
            }
            0x3b => Ok((input, Block::Terminator)),
            _ => unreachable!(),
        }
    }
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let file_name = &args[1];
    let mut file = File::open(file_name)?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    let (input, header) = GifHeader::parse(&data)?;
    println!("Gif Header: {:?}", header);
    let (input, lsd) = LogicalScreenDescriptor::parse(input)?;
    println!("Logical Screen Descriptor: {:?}", lsd);
    let global_color_table_size = 2u8.pow((lsd.packed_fields & 0x7) as u32 + 1);
    let (input, gct) = GlobalColorTable::parse(input, global_color_table_size as usize)?;
    println!("Global Color Table: {:?}", gct);
    let (input, blocks) = many_till(Block::parse, |b| matches!(b, Block::Terminator))(input)?;
    println!("Blocks: {:?}", blocks);
    Ok(())
}