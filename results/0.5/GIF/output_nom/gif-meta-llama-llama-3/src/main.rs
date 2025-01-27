use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::take_till,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{Read, Result},
};

#[derive(Debug)]
enum ColorTable {
    Present(Vec<u8>),
    NotPresent,
}

#[derive(Debug)]
struct Gif87aHeader {
    signature: String,
    version: String,
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    flags: u8,
    bg_color: u8,
    aspect_ratio: u8,
}

#[derive(Debug)]
struct ColorTableDescriptor {
    size: u8,
    table: ColorTable,
}

#[derive(Debug)]
struct GifImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    flags: u8,
    delay: u16,
}

#[derive(Debug)]
enum GraphicControlExtension {
    Present(GifImageDescriptor),
    NotPresent,
}

#[derive(Debug)]
struct GraphicRenderingBlock {
    descriptor: GifImageDescriptor,
    data: Vec<u8>,
}

#[derive(Debug)]
struct GifTrailer {
    byte: u8,
}

fn parse_gif87a_header(input: &[u8]) -> IResult<&[u8], Gif87aHeader> {
    let (input, signature) = take(3u8)(input)?;
    let signature = String::from_utf8_lossy(signature).into_owned();
    let (input, version) = take(3u8)(input)?;
    let version = String::from_utf8_lossy(version).into_owned();
    Ok((input, Gif87aHeader { signature, version }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, width) = take(2u8)(input)?;
    let width = u16::from_be_bytes([width[0], width[1]]);
    let (input, height) = take(2u8)(input)?;
    let height = u16::from_be_bytes([height[0], height[1]]);
    let (input, flags) = take(1u8)(input)?;
    let flags = flags[0];
    let (input, bg_color) = take(1u8)(input)?;
    let bg_color = bg_color[0];
    let (input, aspect_ratio) = take(1u8)(input)?;
    let aspect_ratio = aspect_ratio[0];
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            flags,
            bg_color,
            aspect_ratio,
        },
    ))
}

fn parse_color_table_descriptor(input: &[u8]) -> IResult<&[u8], ColorTableDescriptor> {
    let (input, size) = take(1u8)(input)?;
    let size = size[0];
    let (input, table) = opt(take(size as usize * 3))(input)?;
    let table = match table {
        Some(table) => ColorTable::Present(table.to_vec()),
        None => ColorTable::NotPresent,
    };
    Ok((input, ColorTableDescriptor { size, table }))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, identifier) = tag(&[0x21, 0xf9])(input)?;
    let (input, block_size) = take(1u8)(input)?;
    let block_size = block_size[0] as usize;
    let (input, data) = take(block_size)(input)?;
    let descriptor = GifImageDescriptor {
        left: u16::from_be_bytes([data[1], data[2]]),
        top: u16::from_be_bytes([data[3], data[4]]),
        width: u16::from_be_bytes([data[5], data[6]]),
        height: u16::from_be_bytes([data[7], data[8]]),
        flags: data[9],
        delay: u16::from_be_bytes([data[10], data[11]]),
    };
    Ok((input, GraphicControlExtension::Present(descriptor)))
}

fn parse_graphic_rendering_block(input: &[u8]) -> IResult<&[u8], GraphicRenderingBlock> {
    let (input, identifier) = tag(&[0x21, 0xf9])(input)?;
    let (input, block_size) = take(1u8)(input)?;
    let block_size = block_size[0] as usize;
    let (input, data) = take(block_size)(input)?;
    let descriptor = GifImageDescriptor {
        left: u16::from_be_bytes([data[1], data[2]]),
        top: u16::from_be_bytes([data[3], data[4]]),
        width: u16::from_be_bytes([data[5], data[6]]),
        height: u16::from_be_bytes([data[7], data[8]]),
        flags: data[9],
        delay: u16::from_be_bytes([data[10], data[11]]),
    };
    Ok((input, GraphicRenderingBlock { descriptor, data: data.to_vec() }))
}

fn parse_gif_trailer(input: &[u8]) -> IResult<&[u8], GifTrailer> {
    let (input, byte) = take(1u8)(input)?;
    let byte = byte[0];
    Ok((input, GifTrailer { byte }))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], ()> {
    let (input, header) = parse_gif87a_header(input)?;
    let (input, lsd) = parse_logical_screen_descriptor(input)?;
    let (input, ctd) = parse_color_table_descriptor(input)?;
    let (input, _) = opt(parse_graphic_control_extension)(input)?;
    let (input, _) = parse_graphic_rendering_block(input)?;
    let (input, _) = parse_gif_trailer(input)?;
    Ok((input, ()))
}

fn main() -> Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <gif_file>", args[0]);
        return Ok(());
    }
    let mut file = File::open(&args[1])?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;
    let _ = parse_gif(&data);
    Ok(())
}