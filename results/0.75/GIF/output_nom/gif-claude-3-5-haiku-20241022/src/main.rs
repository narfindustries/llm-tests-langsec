use std::fs::File;
use std::io::Read;
use std::env;
use nom::{
    bytes::complete::{tag, take},
    multi::{count, many0},
    number::complete::{le_u8, le_u16},
    sequence::{tuple, preceded},
    combinator::{opt, map},
    branch::alt,
    IResult,
};

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
struct GlobalColorTable {
    entries: Vec<ColorTableEntry>,
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
struct ImageData {
    lzw_min_code_size: u8,
    data_blocks: Vec<Vec<u8>>,
}

#[derive(Debug)]
enum Extension {
    GraphicControl(GraphicControlExtension),
    PlainText(PlainTextExtension),
    Application(ApplicationExtension),
    Comment(CommentExtension),
}

#[derive(Debug)]
struct GraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct PlainTextExtension {
    text_grid_left_position: u16,
    text_grid_top_position: u16,
    text_grid_width: u16,
    text_grid_height: u16,
    cell_width: u8,
    cell_height: u8,
    foreground_color_index: u8,
    background_color_index: u8,
    text_data: Vec<u8>,
}

#[derive(Debug)]
struct ApplicationExtension {
    identifier: [u8; 8],
    authentication_code: [u8; 3],
    data_blocks: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct CommentExtension {
    comments: Vec<Vec<u8>>,
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<GlobalColorTable>,
    blocks: Vec<GifBlock>,
}

#[derive(Debug)]
enum GifBlock {
    ImageBlock(ImageBlock),
    ExtensionBlock(Extension),
}

#[derive(Debug)]
struct ImageBlock {
    graphic_control: Option<GraphicControlExtension>,
    descriptor: ImageDescriptor,
    local_color_table: Option<GlobalColorTable>,
    image_data: ImageData,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, (signature, version)) = tuple((
        take(3usize),
        take(3usize)
    ))(input)?;

    Ok((input, GifHeader {
        signature: signature.try_into().unwrap(),
        version: version.try_into().unwrap(),
    }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) = tuple((
        le_u16,
        le_u16,
        le_u8,
        le_u8,
        le_u8
    ))(input)?;

    Ok((input, LogicalScreenDescriptor {
        width,
        height,
        packed_fields,
        background_color_index,
        pixel_aspect_ratio,
    }))
}

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], GlobalColorTable> {
    let (input, entries) = count(
        map(tuple((le_u8, le_u8, le_u8)), |(r, g, b)| ColorTableEntry { red: r, green: g, blue: b }),
        size
    )(input)?;

    Ok((input, GlobalColorTable { entries }))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    preceded(tag(&[0x2C]), 
        map(
            tuple((le_u16, le_u16, le_u16, le_u16, le_u8)),
            |(left_position, top_position, width, height, packed_fields)| ImageDescriptor {
                left_position,
                top_position,
                width,
                height,
                packed_fields,
            }
        )
    )(input)
}

fn parse_image_data(input: &[u8]) -> IResult<&[u8], ImageData> {
    let (input, lzw_min_code_size) = le_u8(input)?;
    let (input, data_blocks) = many0(parse_data_block)(input)?;

    Ok((input, ImageData {
        lzw_min_code_size,
        data_blocks,
    }))
}

fn parse_data_block(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let (input, block_size) = le_u8(input)?;
    let (input, block_data) = take(block_size as usize)(input)?;
    
    Ok((input, block_data.to_vec()))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    preceded(tag(&[0x21, 0xF9, 0x04]),
        map(
            tuple((le_u8, le_u16, le_u8, le_u8)),
            |(packed_fields, delay_time, transparent_color_index, _)| GraphicControlExtension {
                packed_fields,
                delay_time,
                transparent_color_index,
            }
        )
    )(input)
}

fn parse_local_color_table(input: &[u8], descriptor: &ImageDescriptor) -> IResult<&[u8], Option<GlobalColorTable>> {
    if descriptor.packed_fields & 0x80 != 0 {
        let color_table_size = 2usize.pow((descriptor.packed_fields & 0x07) as u32 + 1);
        let (input, color_table) = parse_color_table(input, color_table_size)?;
        Ok((input, Some(color_table)))
    } else {
        Ok((input, None))
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let (input, global_color_table) = if logical_screen_descriptor.packed_fields & 0x80 != 0 {
        let color_table_size = 2usize.pow((logical_screen_descriptor.packed_fields & 0x07) as u32 + 1);
        let (input, color_table) = parse_color_table(input, color_table_size)?;
        (input, Some(color_table))
    } else {
        (input, None)
    };

    let (input, blocks) = many0(
        alt((
            map(
                tuple((
                    opt(parse_graphic_control_extension),
                    parse_image_descriptor,
                    |i| parse_local_color_table(i, &parse_image_descriptor(i)?.1),
                    parse_image_data
                )),
                |(graphic_control, descriptor, local_color_table, image_data)| 
                    GifBlock::ImageBlock(ImageBlock {
                        graphic_control,
                        descriptor,
                        local_color_table,
                        image_data,
                    })
            ),
            map(
                parse_graphic_control_extension,
                |ext| GifBlock::ExtensionBlock(Extension::GraphicControl(ext))
            )
        ))
    )(input)?;

    Ok((input, Gif {
        header,
        logical_screen_descriptor,
        global_color_table,
        blocks,
    }))
}

fn main() -> std::io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gif_file>", args[0]);
        std::process::exit(1);
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((_, gif)) => {
            println!("Successfully parsed GIF: {:?}", gif);
            Ok(())
        },
        Err(e) => {
            eprintln!("Failed to parse GIF: {:?}", e);
            std::process::exit(1)
        }
    }
}