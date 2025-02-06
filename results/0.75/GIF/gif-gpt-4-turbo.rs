use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    multi::count,
    number::complete::{le_u16, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct Color {
    r: u8,
    g: u8,
    b: u8,
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
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
struct Block {
    data: Vec<u8>,
}

#[derive(Debug)]
enum Extension {
    GraphicControl(Block),
    Comment(Block),
    PlainText(Block),
    Application(Block),
    Unknown(Block),
}

#[derive(Debug)]
struct Gif {
    header: String,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<Color>>,
    blocks: Vec<Extension>,
}

fn parse_color(input: &[u8]) -> IResult<&[u8], Color> {
    let (input, (r, g, b)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, Color { r, g, b }))
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

fn parse_extension(input: &[u8]) -> IResult<&[u8], Extension> {
    let (input, _) = tag([0x21])(input)?;
    let (input, label) = le_u8(input)?;
    let (input, block_size) = le_u8(input)?;
    let (input, data) = take(block_size)(input)?;

    let block = Block {
        data: data.to_vec(),
    };

    let extension = match label {
        0xF9 => Extension::GraphicControl(block),
        0xFE => Extension::Comment(block),
        0x01 => Extension::PlainText(block),
        0xFF => Extension::Application(block),
        _ => Extension::Unknown(block),
    };

    Ok((input, extension))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, _) = tag("GIF")(input)?;
    let (input, version) = map_res(take(3usize), std::str::from_utf8)(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let (input, global_color_table) = if logical_screen_descriptor.packed_fields & 0b1000_0000 > 0 {
        let table_size = 1 << ((logical_screen_descriptor.packed_fields & 0b0000_0111) + 1);
        map(count(parse_color, table_size as usize), Some)(input)?
    } else {
        (input, None)
    };

    let mut blocks = Vec::new();
    let mut current_input = input;
    while let Ok((new_input, extension)) = parse_extension(current_input) {
        blocks.push(extension);
        current_input = new_input;
    }

    let gif = Gif {
        header: format!("GIF{}", version),
        logical_screen_descriptor,
        global_color_table,
        blocks,
    };

    Ok((current_input, gif))
}

fn read_file_data(filename: &str) -> io::Result<Vec<u8>> {
    let mut file = File::open(filename)?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;
    Ok(buffer)
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let data = read_file_data(filename)?;
    match parse_gif(&data) {
        Ok((_, gif)) => {
            println!("Parsed GIF: {:?}", gif);
        }
        Err(e) => {
            println!("Error parsing GIF: {:?}", e);
        }
    }

    Ok(())
}