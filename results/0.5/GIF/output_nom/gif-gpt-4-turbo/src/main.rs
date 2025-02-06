use nom::{
    bytes::complete::{tag, take},
    combinator::{map_res, opt},
    multi::count,
    number::complete::{le_u16, le_u8},
    sequence::tuple,
    IResult,
};
use std::fs::File;
use std::io::{self, Read};

#[derive(Debug)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
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
struct Block {
    block_type: u8,
    size: u8,
    data: Vec<u8>,
}

#[derive(Debug)]
struct GIF {
    header: String,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<Color>>,
    blocks: Vec<Block>,
    trailer: u8,
}

fn parse_color(input: &[u8]) -> IResult<&[u8], Color> {
    let (input, (red, green, blue)) = tuple((le_u8, le_u8, le_u8))(input)?;
    Ok((input, Color { red, green, blue }))
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

fn parse_global_color_table(input: &[u8], size: usize) -> IResult<&[u8], Vec<Color>> {
    count(parse_color, size)(input)
}

fn parse_block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, (block_type, size)) = tuple((le_u8, le_u8))(input)?;
    let (input, data) = take(size)(input)?;
    Ok((
        input,
        Block {
            block_type,
            size,
            data: data.to_vec(),
        },
    ))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GIF> {
    let (input, _) = tag("GIF")(input)?;
    let (input, version) = map_res(take(3usize), std::str::from_utf8)(input)?;
    let (input, lsd) = parse_logical_screen_descriptor(input)?;

    let global_color_table_flag = (lsd.packed_fields & 0b10000000) >> 7;
    let global_color_table_size = 1 << ((lsd.packed_fields & 0b00000111) + 1);

    let (input, global_color_table) = if global_color_table_flag == 1 {
        let (input, table) = parse_global_color_table(input, global_color_table_size)?;
        (input, Some(table))
    } else {
        (input, None)
    };

    let mut blocks = Vec::new();
    let mut input = input;
    while input[0] != 0x3B {
        let (new_input, block) = parse_block(input)?;
        blocks.push(block);
        input = new_input;
    }

    let (input, trailer) = le_u8(input)?;

    Ok((
        input,
        GIF {
            header: format!("GIF{}", version),
            logical_screen_descriptor: lsd,
            global_color_table,
            blocks,
            trailer,
        },
    ))
}

fn main() -> io::Result<()> {
    let args: Vec<String> = std::env::args().collect();
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
        Ok((_, gif)) => {
            println!("{:?}", gif);
        }
        Err(e) => {
            println!("Failed to parse GIF: {:?}", e);
        }
    }

    Ok(())
}