use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    error::Error,
    multi::count,
    number::complete::{le_u16, le_u8},
    sequence::{preceded, tuple},
    IResult,
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
struct ColorTable(Vec<[u8; 3]>);

#[derive(Debug)]
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct LZWImageData {
    lzw_minimum_code_size: u8,
    image_data_blocks: Vec<Vec<u8>>,
}

#[derive(Debug)]
enum Extension {
    GraphicControl(Vec<u8>),
    Comment(Vec<u8>),
    PlainText(Vec<u8>),
    Application(Vec<u8>),
}

#[derive(Debug)]
enum GifBlock {
    Image {
        descriptor: ImageDescriptor,
        local_color_table: Option<ColorTable>,
        image_data: LZWImageData,
    },
    Extension(Extension),
}

#[derive(Debug)]
struct Gif {
    header: GifHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<ColorTable>,
    blocks: Vec<GifBlock>,
}

fn parse_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, (signature, version)) = tuple((tag(b"GIF"), take(3usize)))(input)?;
    Ok((input, GifHeader {
        signature: [signature[0], signature[1], signature[2]],
        version: [version[0], version[1], version[2]],
    }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
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

fn parse_color_table(size: usize) -> impl Fn(&[u8]) -> IResult<&[u8], ColorTable> {
    move |input: &[u8]| {
        let (input, colors) = count(take(3usize), size)(input)?;
        let colors: Vec<[u8; 3]> = colors.into_iter().map(|c| [c[0], c[1], c[2]]).collect();
        Ok((input, ColorTable(colors)))
    }
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (_, left, top, width, height, packed_fields)) = tuple((
        tag(b","),
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u8,
    ))(input)?;
    Ok((
        input,
        ImageDescriptor {
            left,
            top,
            width,
            height,
            packed_fields,
        },
    ))
}

fn parse_lzw_image_data(input: &[u8]) -> IResult<&[u8], LZWImageData> {
    let (input, lzw_minimum_code_size) = le_u8(input)?;
    let mut input = input;
    let mut image_data_blocks = Vec::new();

    loop {
        let (new_input, block_size) = le_u8(input)?;
        if block_size == 0 {
            break;
        }
        let (new_input, block_data) = take(block_size)(new_input)?;
        image_data_blocks.push(block_data.to_vec());
        input = new_input;
    }

    Ok((
        input,
        LZWImageData {
            lzw_minimum_code_size,
            image_data_blocks,
        },
    ))
}

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, _) = tag(b"!")(input)?;
    let (input, label) = le_u8(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, data) = take(block_size)(input)?;

    let extension = match label {
        0xF9 => Extension::GraphicControl(data.to_vec()),
        0xFE => Extension::Comment(data.to_vec()),
        0x01 => Extension::PlainText(data.to_vec()),
        0xFF => Extension::Application(data.to_vec()),
        _ => return Err(nom::Err::Error(Error::new(input, nom::error::ErrorKind::Tag))),
    };

    let (input, _) = tag(b"\x00")(input)?;
    Ok((input, extension))
}

fn parse_gif_block(input: &[u8]) -> IResult<&[u8], GifBlock> {
    let (input, block) = preceded(opt(tag(b",")), |input| {
        let (input, descriptor) = parse_image_descriptor(input)?;
        let has_local_color_table = descriptor.packed_fields & 0b1000_0000 != 0;
        let size_of_local_color_table = 1 << ((descriptor.packed_fields & 0b0000_0111) + 1);

        let (input, local_color_table) = if has_local_color_table {
            map(parse_color_table(size_of_local_color_table), Some)(input)?
        } else {
            (input, None)
        };

        let (input, image_data) = parse_lzw_image_data(input)?;

        Ok((
            input,
            GifBlock::Image {
                descriptor,
                local_color_table,
                image_data,
            },
        ))
    })(input)?;

    Ok((input, block))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = parse_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let has_global_color_table =
        logical_screen_descriptor.packed_fields & 0b1000_0000 != 0;
    let size_of_global_color_table =
        1 << ((logical_screen_descriptor.packed_fields & 0b0000_0111) + 1);

    let (input, global_color_table) = if has_global_color_table {
        map(parse_color_table(size_of_global_color_table), Some)(input)?
    } else {
        (input, None)
    };

    let mut input = input;
    let mut blocks = Vec::new();

    while !input.is_empty() {
        if let Ok((new_input, block)) = parse_gif_block(input) {
            blocks.push(block);
            input = new_input;
        } else if let Ok((new_input, extension)) = parse_extension(input) {
            blocks.push(GifBlock::Extension(extension));
            input = new_input;
        } else {
            break;
        }
    }

    Ok((
        input,
        Gif {
            header,
            logical_screen_descriptor,
            global_color_table,
            blocks,
        },
    ))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <file>", args[0]);
        return;
    }

    let filename = &args[1];
    let data = fs::read(filename).expect("Failed to read file");

    match parse_gif(&data) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }
}