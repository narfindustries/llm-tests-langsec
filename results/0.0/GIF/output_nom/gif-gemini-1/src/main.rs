use nom::{
    bytes::complete::{tag, take},
    combinator::opt,
    error::ErrorKind,
    number::complete::{le_u16, le_u8},
    sequence::{tuple, preceded},
    IResult,
};
use std::fs::File;
use std::io::Read;
use std::path::Path;

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 6],
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<Vec<[u8; 3]>>,
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: PackedFields,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct PackedFields {
    global_color_table_flag: bool,
    color_resolution: u8,
    sort_flag: bool,
    global_color_table_size: u8,
}

#[derive(Debug)]
struct ImageDescriptor {
    image_left_position: u16,
    image_top_position: u16,
    image_width: u16,
    image_height: u16,
    packed_fields: PackedFields,
    lzw_minimum_code_size: u8,
    local_color_table: Option<Vec<[u8; 3]>>,
    image_data: Vec<u8>,
}

#[derive(Debug)]
enum Extension {
    GraphicControlExtension {
        packed_fields: u8,
        delay_time: u16,
        transparent_color_index: u8,
        terminator: u8,
    },
    CommentExtension {
        comment: String,
    },
    ApplicationExtension {
        application_identifier: String,
        authentication_code: String,
        data: Vec<u8>,
    },
    // Add other extension types as needed
}

fn parse_packed_fields(input: &[u8]) -> IResult<&[u8], PackedFields> {
    let (input, packed_fields) = le_u8(input)?;
    Ok((
        input,
        PackedFields {
            global_color_table_flag: (packed_fields >> 7) & 1 == 1,
            color_resolution: (packed_fields >> 4) & 7,
            sort_flag: (packed_fields >> 3) & 1 == 1,
            global_color_table_size: packed_fields & 7,
        },
    ))
}

fn parse_color_table(input: &[u8], size: u8) -> IResult<&[u8], Vec<[u8; 3]>> {
    let num_entries = 1usize << (size + 1);
    let (input, table) = take(num_entries * 3)(input)?;
    let table = table.chunks_exact(3).map(|chunk| [chunk[0], chunk[1], chunk[2]]).collect();
    Ok((input, table))
}

fn parse_image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, (left, top, width, height, packed_fields, lzw_minimum_code_size)) = tuple((
        le_u16,
        le_u16,
        le_u16,
        le_u16,
        parse_packed_fields,
        le_u8,
    ))(input)?;

    let (input, local_color_table) = opt(preceded(tag("!"), |i| {
        let (i, packed_fields) = parse_packed_fields(i)?;
        let num_entries = 1usize << (packed_fields.global_color_table_size + 1);
        let (i, table) = take(num_entries * 3)(i)?;
        let table = table.chunks_exact(3).map(|chunk| [chunk[0], chunk[1], chunk[2]]).collect();
        Ok((i, table))
    }))(input)?;

    let (input, image_data) = take_while1(|b| b != &0x3B)(input)?;

    Ok((
        input,
        ImageDescriptor {
            image_left_position: left,
            image_top_position: top,
            image_width: width,
            image_height: height,
            packed_fields,
            lzw_minimum_code_size,
            local_color_table,
            image_data: image_data.to_vec(),
        },
    ))
}

fn parse_gif(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = tag("GIF89a")(input)?;
    let (input, logical_screen_descriptor) = parse_logical_screen_descriptor(input)?;
    let (input, global_color_table) = opt(|i| {
        let (i, packed_fields) = parse_packed_fields(i)?;
        parse_color_table(i, packed_fields.global_color_table_size)
    })(input)?;

    Ok((
        input,
        GifHeader {
            signature: signature.try_into().unwrap(),
            logical_screen_descriptor,
            global_color_table,
        },
    ))
}

fn parse_logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields, background_color_index, pixel_aspect_ratio)) =
        tuple((le_u16, le_u16, parse_packed_fields, le_u8, le_u8))(input)?;
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

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let mut file = match File::open(&path) {
        Ok(file) => file,
        Err(err) => {
            eprintln!("Error opening file: {}", err);
            std::process::exit(1);
        }
    };

    let mut buffer = Vec::new();
    match file.read_to_end(&mut buffer) {
        Ok(_) => (),
        Err(err) => {
            eprintln!("Error reading file: {}", err);
            std::process::exit(1);
        }
    };

    match parse_gif(&buffer) {
        Ok((_, gif)) => println!("{:#?}", gif),
        Err(e) => eprintln!("Error parsing GIF: {:?}", e),
    }
}

fn take_while1(predicate: fn(&u8) -> bool) -> impl Fn(&[u8]) -> IResult<&[u8], &[u8]> {
    move |input: &[u8]| {
        let mut len = 0;
        for i in input {
            if predicate(i) {
                len += 1;
            } else {
                break;
            }
        }
        if len == 0 {
            Err(nom::Err::Error(nom::error::Error {
                input: input,
                code: ErrorKind::TakeWhile1,
            }))
        } else {
            Ok((&input[len..], &input[..len]))
        }
    }
}