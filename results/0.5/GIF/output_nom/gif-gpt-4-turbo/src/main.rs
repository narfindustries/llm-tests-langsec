use nom::{
    bytes::complete::{tag, take},
    combinator::{map_res, opt},
    multi::count,
    number::complete::{le_u16, le_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::{env, fs::File, io::Read};

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
    bg_color_index: u8,
    pixel_aspect_ratio: u8,
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
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
enum Block {
    ImageDescriptor(ImageDescriptor),
    GraphicControlExtension(GraphicControlExtension),
    CommentExtension(Vec<u8>),
    PlainTextExtension(Vec<u8>),
    ApplicationExtension(Vec<u8>),
    Unknown,
}

#[derive(Debug)]
struct Gif {
    header: Vec<u8>,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<Color>>,
    blocks: Vec<Block>,
}

fn parse_color(input: &[u8]) -> IResult<&[u8], Color> {
    let (input, (r, g, b)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, Color { r, g, b }))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, bg_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, le_u8, le_u8, le_u8))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
            bg_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (_, left_position, top_position, width, height, packed_fields)) = tuple((
        tag("2C"),
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        le_u8,
    ))(input)?;
    Ok((
        input,
        ImageDescriptor {
            left_position,
            top_position,
            width,
            height,
            packed_fields,
        },
    ))
}

fn parse_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, (_, packed_fields, delay_time, transparent_color_index, _)) = tuple((
        tag("21F9"),
        le_u8,
        le_u16,
        le_u8,
        le_u8,
    ))(input)?;
    Ok((
        input,
        GraphicControlExtension {
            packed_fields,
            delay_time,
            transparent_color_index,
        },
    ))
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, block_label) = take(2usize)(input)?;
    match block_label {
        b"2C" => {
            let (input, image_descriptor) = parse_image_descriptor(input)?;
            Ok((input, Block::ImageDescriptor(image_descriptor)))
        }
        b"21F9" => {
            let (input, gce) = parse_graphic_control_extension(input)?;
            Ok((input, Block::GraphicControlExtension(gce)))
        }
        b"21FE" => {
            let (input, data) = preceded(tag("21FE"), take_until_terminator)(input)?;
            Ok((input, Block::CommentExtension(data.to_vec())))
        }
        _ => Ok((input, Block::Unknown)),
    }
}

fn take_until_terminator(input: &[u8]) -> IResult<&[u8], &[u8]> {
    let pos = input.iter().position(|&r| r == 0x00);
    match pos {
        Some(p) => Ok((&input[p + 1..], &input[..p])),
        None => Err(nom::Err::Incomplete(nom::Needed::Unknown)),
    }
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, header) = tag("GIF")(input)?;
    let (input, version) = take(3usize)(input)?;
    let (input, lsd) = parse_logical_screen_descriptor(input)?;
    let (input, gct) = opt(count(parse_color, 256))(input)?;
    let (input, blocks) = count(parse_block, 10)(input)?;

    Ok((
        input,
        Gif {
            header: [header, version].concat(),
            logical_screen_descriptor: lsd,
            global_color_table: gct,
            blocks,
        },
    ))
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err("Usage: gif_parser <path_to_gif_file>".into());
    }

    let mut file = File::open(&args[1])?;
    let mut contents = Vec::new();
    file.read_to_end(&mut contents)?;

    match parse_gif(&contents) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => println!("Failed to parse GIF: {:?}", e),
    }

    Ok(())
}