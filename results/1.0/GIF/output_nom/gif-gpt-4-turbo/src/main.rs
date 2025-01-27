use nom::{
    bytes::complete::{tag, take},
    number::complete::{le_u16, le_u8},
    sequence::tuple,
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct LogicalScreenDescriptor {
    logical_screen_width: u16,
    logical_screen_height: u16,
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
enum Block {
    ImageBlock(ImageDescriptor),
    ExtensionBlock(Extension),
    Trailer,
}

#[derive(Debug)]
struct ImageDescriptor {
    image_left_position: u16,
    image_top_position: u16,
    image_width: u16,
    image_height: u16,
    packed_fields: u8,
}

#[derive(Debug)]
enum Extension {
    GraphicControl,
    Comment,
    PlainText,
    Application,
}

#[derive(Debug)]
struct GIF {
    header: String,
    version: String,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<GlobalColorTable>,
    blocks: Vec<Block>,
}

fn parse_color_table_entry(input: &[u8]) -> IResult<&[u8], ColorTableEntry> {
    let (input, (red, green, blue)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, ColorTableEntry { red, green, blue }))
}

fn parse_global_color_table(input: &[u8], size: usize) -> IResult<&[u8], GlobalColorTable> {
    let (input, entries) = nom::multi::count(parse_color_table_entry, size)(input)?;
    Ok((input, GlobalColorTable { entries }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (logical_screen_width, logical_screen_height, packed_fields, background_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            logical_screen_width,
            logical_screen_height,
            packed_fields,
            background_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GIF> {
    let (input, _) = tag("GIF")(input)?;
    let (input, version) = nom::bytes::complete::take(3usize)(input)?;
    let (input, lsd) = parse_logical_screen_descriptor(input)?;

    let global_color_table_flag = (lsd.packed_fields & 0b10000000) >> 7;
    let size_of_gct = 2usize.pow(((lsd.packed_fields & 0b00000111) + 1) as u32);

    let (input, global_color_table) = if global_color_table_flag == 1 {
        let (input, gct) = parse_global_color_table(input, size_of_gct)?;
        (input, Some(gct))
    } else {
        (input, None)
    };

    let (input, blocks) = nom::multi::many0(parse_block)(input)?;

    let gif = GIF {
        header: "GIF".to_string(),
        version: String::from_utf8(version.to_vec()).unwrap(),
        logical_screen_descriptor: lsd,
        global_color_table,
        blocks,
    };

    Ok((input, gif))
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, label) = le_u8(input)?;
    match label {
        0x2C => {
            let (input, (image_left_position, image_top_position, image_width, image_height, packed_fields)) =
                tuple((le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
            Ok((
                input,
                Block::ImageBlock(ImageDescriptor {
                    image_left_position,
                    image_top_position,
                    image_width,
                    image_height,
                    packed_fields,
                }),
            ))
        }
        0x21 => {
            // This is a simplified example assuming only graphic control extensions appear in GIF
            let (input, _) = tag(&[0xF9, 0x04])(input)?;
            let (input, _) = take(5usize)(input)?; // Skipping the actual parsing for simplicity
            Ok((input, Block::ExtensionBlock(Extension::GraphicControl)))
        }
        0x3B => Ok((input, Block::Trailer)),
        _ => Err(nom::Err::Error((input, nom::error::ErrorKind::Tag))),
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "Usage: gif_parser <filename>",
        ));
    }

    let mut file = File::open(&args[1])?;
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer)?;

    match parse_gif(&buffer) {
        Ok((_rest, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Failed to parse GIF: {:?}", e),
    }

    Ok(())
}