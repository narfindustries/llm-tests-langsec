use nom::{
    bytes::complete::{tag, take},
    multi::{count, many0, many1},
    number::complete::{le_u8, le_u16},
    sequence::{preceded, tuple},
    IResult, combinator::{map, flat_map},
};
use std::env;
use std::fs;

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 3],
    version: [u8; 3],
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
struct ColorTableEntry {
    red: u8,
    green: u8,
    blue: u8,
}

#[derive(Debug)]
struct ImageDescriptor {
    left_position: u16,
    top_position: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct GraphicControlExtension {
    block_size: u8,
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    images: Vec<Image>,
    trailer: u8,
}

#[derive(Debug)]
struct Image {
    descriptor: ImageDescriptor,
    local_color_table: Option<Vec<ColorTableEntry>>,
    lzw_min_code_size: u8,
    image_data: Vec<u8>,
    graphic_control_extension: Option<GraphicControlExtension>,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = take(3usize)(input)?;
    let (input, version) = take(3usize)(input)?;
    
    Ok((input, GifHeader {
        signature: signature.try_into().unwrap(),
        version: version.try_into().unwrap(),
    }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, background_color_index) = le_u8(input)?;
    let (input, pixel_aspect_ratio) = le_u8(input)?;
    
    Ok((input, LogicalScreenDescriptor {
        width,
        height,
        packed_fields,
        background_color_index,
        pixel_aspect_ratio,
    }))
}

fn parse_color_table(input: &[u8], table_size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    let (input, entries) = count(
        map(
            tuple((le_u8, le_u8, le_u8)), 
            |(r, g, b)| ColorTableEntry { red: r, green: g, blue: b }
        ),
        table_size
    )(input)?;
    
    Ok((input, entries))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag(&[0x21, 0xF9])(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, delay_time) = le_u16(input)?;
    let (input, transparent_color_index) = le_u8(input)?;
    let (input, _) = le_u8(input)?; // Block terminator
    
    Ok((input, GraphicControlExtension {
        block_size,
        packed_fields,
        delay_time,
        transparent_color_index,
    }))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag(&[0x2C])(input)?;
    let (input, left_position) = le_u16(input)?;
    let (input, top_position) = le_u16(input)?;
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
    
    Ok((input, ImageDescriptor {
        left_position,
        top_position,
        width,
        height,
        packed_fields,
    }))
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], (u8, Vec<u8>)> {
    let (input, lzw_min_code_size) = le_u8(input)?;
    let (input, data_blocks) = many1(
        flat_map(
            le_u8,
            |size| take(size as usize)
        )
    )(input)?;
    
    Ok((input, (lzw_min_code_size, data_blocks.concat())))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_gif_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    
    let (input, global_color_table) = if logical_screen_descriptor.packed_fields & 0x80 != 0 {
        let table_size = 2usize.pow((logical_screen_descriptor.packed_fields & 0x07) as u32 + 1);
        let (input, table) = parse_color_table(input, table_size)?;
        (input, Some(table))
    } else {
        (input, None)
    };
    
    let (input, images) = many0(|i| {
        let (i, graphic_control_extension) = nom::combinator::opt(parse_graphic_control_extension)(i)?;
        let (i, descriptor) = parse_image_descriptor(i)?;
        
        let (i, local_color_table) = if descriptor.packed_fields & 0x80 != 0 {
            let table_size = 2usize.pow((descriptor.packed_fields & 0x07) as u32 + 1);
            let (i, table) = parse_color_table(i, table_size)?;
            (i, Some(table))
        } else {
            (i, None)
        };
        
        let (i, (lzw_min_code_size, image_data)) = parse_image_data(i)?;
        
        Ok((i, Image {
            descriptor,
            local_color_table,
            lzw_min_code_size,
            image_data,
            graphic_control_extension,
        }))
    })(input)?;
    
    let (input, trailer) = le_u8(input)?;
    
    Ok((input, Gif {
        header,
        logical_screen_descriptor,
        global_color_table,
        images,
        trailer,
    }))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <gif_file>", args[0]);
        std::process::exit(1);
    }
    
    let file_contents = fs::read(&args[1])?;
    match parse_gif(&file_contents) {
        Ok((_, gif)) => {
            println!("Parsed GIF: {:?}", gif);
            Ok(())
        }
        Err(e) => {
            eprintln!("Error parsing GIF: {:?}", e);
            std::process::exit(1);
        }
    }
}