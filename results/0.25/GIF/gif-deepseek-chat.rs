use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u8},
    sequence::tuple,
    IResult,
};
use std::{
    env,
    fs::File,
    io::{self, Read},
};

#[derive(Debug)]
struct GIFHeader {
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
struct ColorTable {
    colors: Vec<(u8, u8, u8)>,
}

#[derive(Debug)]
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct GIF {
    header: GIFHeader,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<ColorTable>,
    frames: Vec<ImageDescriptor>,
}

fn parse_gif_header(input: &[u8]) -> IResult<&[u8], GIFHeader> {
    let (input, (signature, version)) = tuple((take(3usize), take(3usize)))(input)?;
    Ok((
        input,
        GIFHeader {
            signature: String::from_utf8_lossy(signature).to_string(),
            version: String::from_utf8_lossy(version).to_string(),
        },
    ))
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

fn parse_color_table(input: &[u8], size: usize) -> IResult<&[u8], ColorTable> {
    let (input, colors) = take(size * 3)(input)?;
    let colors = colors
        .chunks(3)
        .map(|chunk| (chunk[0], chunk[1], chunk[2]))
        .collect();
    Ok((input, ColorTable { colors }))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (left, top, width, height, packed_fields)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
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

fn parse_gif(input: &[u8]) -> IResult<&[u8], GIF> {
    let (input, header) = parse_gif_header(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;

    let global_color_table = if logical_screen_descriptor.packed_fields & 0x80 != 0 {
        let size = 1 << ((logical_screen_descriptor.packed_fields & 0x07) + 1);
        let (input, color_table) = parse_color_table(input, size)?;
        (input, Some(color_table))
    } else {
        (input, None)
    };

    let mut frames = Vec::new();
    let mut input = global_color_table.0;

    while !input.is_empty() {
        if input[0] == 0x2C {
            let (remaining, image_descriptor) = parse_image_descriptor(&input[1..])?;
            frames.push(image_descriptor);
            input = remaining;
        } else if input[0] == 0x21 {
            input = &input[1..];
        } else if input[0] == 0x3B {
            break;
        } else {
            input = &input[1..];
        }
    }

    Ok((
        input,
        GIF {
            header,
            logical_screen_descriptor,
            global_color_table: global_color_table.1,
            frames,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <gif file>", args[0]);
        return Ok(());
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }

    Ok(())
}