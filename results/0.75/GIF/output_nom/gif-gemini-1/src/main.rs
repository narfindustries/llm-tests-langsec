use std::env;
use std::fs;
use std::error::Error;
use nom::{
    IResult,
    bytes::complete::{take, tag},
    number::complete::{le_u16, le_u32},
    sequence::tuple,
    multi::count,
};

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 6],
    version: [u8; 3],
}

#[derive(Debug)]
struct GifScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct GifImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
    local_color_table_flag: bool,
    lct_size: u8,
}


#[derive(Debug)]
struct GifColorTable {
    colors: Vec<[u8;3]>,
}

#[derive(Debug)]
enum GifBlock {
    GraphicControlExtension(u8, u16, u8, u8),
    CommentExtension(Vec<u8>),
    PlainTextExtension(u16, u16, u16, u8, Vec<u8>),
    ApplicationExtension(Vec<u8>),
    OtherExtension(Vec<u8>),
    ImageData(Vec<u8>),

}


fn gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = tag("GIF89a")(input)?;
    Ok((input, GifHeader { signature: *signature.try_into().unwrap(), version: [0;3] }))
}

fn gif_screen_descriptor(input: &[u8]) -> IResult<&[u8], GifScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) = tuple((le_u16, le_u16, take(1u8), take(1u8), take(1u8)))(input)?;
    Ok((input, GifScreenDescriptor { width, height, packed_fields: packed_fields[0], background_color_index: background_color_index[0], pixel_aspect_ratio: pixel_aspect_ratio[0]}))
}


fn gif_global_color_table(input: &[u8], size: u8) -> IResult<&[u8], GifColorTable> {
    let num_colors = 1 << (size + 1);
    let (input, colors) = count(take(3u8), num_colors as usize)(input)?;
    Ok((input, GifColorTable { colors }))
}

fn gif_image_descriptor(input: &[u8]) -> IResult<&[u8], GifImageDescriptor> {
    let (input, (left, top, width, height, packed_fields)) = tuple((le_u16, le_u16, le_u16, le_u16, take(1u8)))(input)?;
    let local_color_table_flag = (packed_fields[0] >> 7) & 1 != 0;
    let lct_size = (packed_fields[0] >> 4) & 7;
    Ok((input, GifImageDescriptor { left, top, width, height, packed_fields: packed_fields[0], local_color_table_flag, lct_size }))
}

fn gif_block(input: &[u8]) -> IResult<&[u8], GifBlock> {
    //Simplified for brevity, needs full extension handling
    Ok((input, GifBlock::ImageData(input.to_vec())))
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: gif_parser <filename>");
        return Ok(());
    }

    let filename = &args[1];
    let data = fs::read(filename)?;

    let (_, header) = gif_header(&data)?;
    println!("Header: {:?}", header);

    let (_, screen_descriptor) = gif_screen_descriptor(&data[6..])?;
    println!("Screen Descriptor: {:?}", screen_descriptor);

    //Global Color Table (if present)
    let global_color_table_size = (screen_descriptor.packed_fields >> 4) & 7;
    let data_after_screen_descriptor = &data[13..];
    if global_color_table_size > 0 {
        let (_, global_color_table) = gif_global_color_table(data_after_screen_descriptor, global_color_table_size)?;
        println!("Global Color Table: {:?}", global_color_table);
    }

    //Image Data and other blocks
    let mut remaining_data = data_after_screen_descriptor;
    if global_color_table_size > 0 {
        let color_table_size = (1 << (global_color_table_size + 1)) * 3;
        remaining_data = &remaining_data[color_table_size..];
    }

    loop {
        match gif_image_descriptor(remaining_data) {
            Ok((remaining, image_descriptor)) => {
                println!("Image Descriptor: {:?}", image_descriptor);
                let mut image_data_end = remaining;
                if image_descriptor.local_color_table_flag {
                    let (_, local_color_table) = gif_global_color_table(remaining, image_descriptor.lct_size)?;
                    let local_color_table_size = (1 << (image_descriptor.lct_size + 1)) * 3;
                    image_data_end = &remaining[local_color_table_size..];
                    println!("Local Color Table: {:?}", local_color_table);
                }

                let (_, block) = gif_block(image_data_end)?;
                println!("Block: {:?}", block);
                remaining_data = image_data_end;
            }
            Err(_) => break,
        }
    }

    Ok(())
}
