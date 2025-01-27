use std::env;
use std::fs;
use std::error::Error;
use nom::{
    IResult,
    bytes::complete::{take, tag},
    number::complete::{be_u16, be_u32, le_u16},
    multi::count,
    sequence::tuple,
    combinator::{map, map_res, opt, all_consuming},
};

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 6],
    version: [u8; 3],
}

#[derive(Debug)]
struct GifLogicalScreenDescriptor {
    width: u16,
    height: u16,
    flags: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct GifGlobalColorTable {
    entries: Vec<[u8; 3]>,
}

#[derive(Debug)]
struct GifGraphicControlExtension {
    block_size: u8,
    flags: u8,
    delay_time: u16,
    transparent_color_index: u8,
    terminator: u8,
}


#[derive(Debug)]
struct GifImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    flags: u8,
    local_color_table_flag: bool,
}

#[derive(Debug)]
struct GifLocalColorTable {
    entries: Vec<[u8; 3]>,
}

#[derive(Debug)]
struct GifImageData {
    data: Vec<u8>,
}

#[derive(Debug)]
struct GifTrailer {
    terminator: u8,
}

fn gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    map(
        tuple((tag(b"GIF87a"), tag(b"GIF89a"))),
        |(signature, version)| GifHeader {
            signature: [signature[0], signature[1], signature[2], signature[3], signature[4], signature[5]],
            version: [version[0], version[1], version[2]]
        }
    )(input)
}

fn gif_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], GifLogicalScreenDescriptor> {
    map(
        tuple((be_u16, be_u16, u8, u8, u8)),
        |(width, height, flags, background_color_index, pixel_aspect_ratio)| GifLogicalScreenDescriptor {
            width,
            height,
            flags,
            background_color_index,
            pixel_aspect_ratio,
        },
    )(input)
}

fn gif_global_color_table(input: &[u8], num_colors: usize) -> IResult<&[u8], GifGlobalColorTable> {
    map(
        count(take(3usize), num_colors),
        |entries| GifGlobalColorTable { entries },
    )(input)
}

fn gif_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GifGraphicControlExtension> {
    map(
        tuple((u8, u8, le_u16, u8, u8)),
        |(block_size, flags, delay_time, transparent_color_index, terminator)| GifGraphicControlExtension {
            block_size,
            flags,
            delay_time,
            transparent_color_index,
            terminator,
        },
    )(input)
}

fn gif_image_descriptor(input: &[u8]) -> IResult<&[u8], GifImageDescriptor> {
    map(
        tuple((be_u16, be_u16, be_u16, be_u16, u8)),
        |(left, top, width, height, flags)| GifImageDescriptor {
            left,
            top,
            width,
            height,
            flags,
            local_color_table_flag: (flags >> 0) & 1 != 0,
        },
    )(input)
}

fn gif_local_color_table(input: &[u8], num_colors: usize) -> IResult<&[u8], GifLocalColorTable> {
    map(
        count(take(3usize), num_colors),
        |entries| GifLocalColorTable { entries },
    )(input)
}

fn gif_image_data(input: &[u8]) -> IResult<&[u8], GifImageData> {
    map(
        map_res(
            take(1usize),
            |bytes| {
                let mut data = Vec::new();
                data.extend_from_slice(bytes);
                Ok(data)
            },
        ),
        |data| GifImageData { data },
    )(input)
}

fn gif_trailer(input: &[u8]) -> IResult<&[u8], GifTrailer> {
    map(tag(b";"), |_| GifTrailer { terminator: b';' })(input)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: gif_parser <filename>");
        return Ok(());
    }

    let filename = &args[1];
    let data = fs::read(filename)?;

    let result = all_consuming(gif_header)(&data);

    match result {
        Ok((_, header)) => println!("GIF Header: {:?}", header),
        Err(e) => eprintln!("Error parsing GIF header: {}", e),
    }

    Ok(())
}
