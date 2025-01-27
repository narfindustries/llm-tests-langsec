use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::many0,
    number::complete::{le_u16, le_u8},
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct GifHeader {
    signature: String,
    version: String,
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
    image_left: u16,
    image_top: u16,
    image_width: u16,
    image_height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
enum Block {
    Extension(ExtensionBlock),
    Image(ImageData),
}

#[derive(Debug)]
struct ExtensionBlock {
    label: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct ImageData {
    descriptor: ImageDescriptor,
    local_color_table: Option<Vec<ColorTableEntry>>,
    lzw_min_code_size: u8,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct GifData {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<ColorTableEntry>>,
    blocks: Vec<Block>,
}

fn parse_color_table_entry(input: &[u8]) -> IResult<&[u8], ColorTableEntry> {
    let (input, red) = le_u8(input)?;
    let (input, green) = le_u8(input)?;
    let (input, blue) = le_u8(input)?;
    Ok((input, ColorTableEntry { red, green, blue }))
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = map_res(take(3usize), std::str::from_utf8)(input)?;
    let (input, version) = map_res(take(3usize), std::str::from_utf8)(input)?;
    Ok((
        input,
        GifHeader {
            signature: signature.to_string(),
            version: version.to_string(),
        },
    ))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, width) = le_u16(input)?;
    let (input, height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, background_color_index) = le_u8(input)?;
    let (input, pixel_aspect_ratio) = le_u8(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
            background_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<ColorTableEntry>> {
    many0(parse_color_table_entry)(input).map(|(i, vec)| (i, vec.into_iter().take(size).collect()))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, image_left) = le_u16(input)?;
    let (input, image_top) = le_u16(input)?;
    let (input, image_width) = le_u16(input)?;
    let (input, image_height) = le_u16(input)?;
    let (input, packed_fields) = le_u8(input)?;
    Ok((
        input,
        ImageDescriptor {
            image_left,
            image_top,
            image_width,
            image_height,
            packed_fields,
        },
    ))
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, block_label) = le_u8(input)?;
    match block_label {
        0x2C => {
            let (input, descriptor) = parse_image_descriptor(input)?;
            let (input, local_color_table) = if descriptor.packed_fields & 0x80 > 0 {
                let size = 2usize.pow(((descriptor.packed_fields & 0x07) + 1) as u32);
                let (input, table) = parse_color_table(input, size)?;
                (input, Some(table))
            } else {
                (input, None)
            };
            let (input, lzw_min_code_size) = le_u8(input)?;
            let (input, image_data) = map(take(1usize), |data: &[u8]| data.to_vec())(input)?;
            Ok((
                input,
                Block::Image(ImageData {
                    descriptor,
                    local_color_table,
                    lzw_min_code_size,
                    image_data,
                }),
            ))
        }
        _ => {
            let (input, data) = map(take(1usize), |data: &[u8]| data.to_vec())(input)?;
            Ok((input, Block::Extension(ExtensionBlock { label: block_label, data })))
        }
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GifData> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let (input, global_color_table) = if logical_screen_descriptor.packed_fields & 0x80 > 0 {
        let size = 2usize.pow(((logical_screen_descriptor.packed_fields & 0x07) + 1) as u32);
        parse_color_table(input, size)?
    } else {
        (input, vec![])
    };
    let (input, blocks) = many0(parse_block)(input)?;
    Ok((
        input,
        GifData {
            header,
            logical_screen_descriptor,
            global_color_table: if !global_color_table.is_empty() {
                Some(global_color_table)
            } else {
                None
            },
            blocks,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Please provide exactly one argument which is the file path.",
        ));
    }
    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((_remaining, gif_data)) => {
            println!("{:?}", gif_data);
        }
        Err(e) => {
            println!("Failed to parse GIF: {:?}", e);
        }
    }

    Ok(())
}