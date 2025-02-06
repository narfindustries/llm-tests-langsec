use nom::{
    bytes::complete::{tag, take},
    combinator::{map_res, opt, rest},
    multi::count,
    number::complete::{le_u16, le_u8},
    sequence::{tuple},
    IResult,
};
use std::fs::read;
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
    packed_fields: u8,
}

#[derive(Debug)]
struct ImageDescriptor {
    image_left_position: u16,
    image_top_position: u16,
    image_width: u16,
    image_height: u16,
    packed_fields: u8,
    local_color_table: Option<Vec<[u8; 3]>>,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct GraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

#[derive(Debug)]
struct CommentExtension {
    comment: String,
}

#[derive(Debug)]
struct ApplicationExtension {
    identifier: String,
    authentication_code: String,
    data: Vec<u8>,
}

fn gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = take(6usize)(input)?;
    let (input, logical_screen_descriptor) = logical_screen_descriptor(input)?;
    let (input, global_color_table) = opt(global_color_table)(input)?;
    Ok((
        input,
        GifHeader {
            signature: signature.try_into().unwrap(),
            logical_screen_descriptor,
            global_color_table,
        },
    ))
}

fn logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, packed_fields)) = tuple((le_u16, le_u16, le_u8))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            packed_fields,
        },
    ))
}

fn global_color_table(input: &[u8]) -> IResult<&[u8], Vec<[u8; 3]>> {
    let (input, size) = le_u8(input)?;
    let num_colors = 1 << (size + 1);
    let (input, color_table) = count(take(3usize), num_colors as usize)(input)?;
    Ok((input, color_table.iter().map(|&x| x.try_into().unwrap()).collect()))
}

fn image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, _) = tag(b"\x2C")(input)?;
    let (input, (image_left_position, image_top_position, image_width, image_height, packed_fields)) =
        tuple((le_u16, le_u16, le_u16, le_u16, le_u8))(input)?;
    let (input, local_color_table) = opt(global_color_table)(input)?;
    let (input, image_data) = rest(input)?;
    Ok((
        input,
        ImageDescriptor {
            image_left_position,
            image_top_position,
            image_width,
            image_height,
            packed_fields,
            local_color_table,
            image_data: image_data.to_vec(),
        },
    ))
}

fn graphic_control_extension(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
    let (input, _) = tag(b"\x21\xF9\x04")(input)?;
    let (input, packed_fields) = le_u8(input)?;
    let (input, delay_time) = le_u16(input)?;
    let (input, transparent_color_index) = le_u8(input)?;
    Ok((
        input,
        GraphicControlExtension {
            packed_fields,
            delay_time,
            transparent_color_index,
        },
    ))
}

fn comment_extension(input: &[u8]) -> IResult<&[u8], CommentExtension> {
    let (input, _) = tag(b"\x21\xFE")(input)?;
    let (input, comment_data) = rest(input)?;
    let comment = String::from_utf8_lossy(comment_data).into_owned();
    Ok((input, CommentExtension { comment }))
}

fn application_extension(input: &[u8]) -> IResult<&[u8], ApplicationExtension> {
    let (input, _) = tag(b"\x21\xFF")(input)?;
    let (input, identifier) = map_res(take(8usize), |bytes: &[u8]| {
        String::from_utf8(bytes.to_vec())
    })(input)?;
    let (input, authentication_code) = map_res(take(3usize), |bytes: &[u8]| {
        String::from_utf8(bytes.to_vec())
    })(input)?;
    let (input, data) = rest(input)?;
    Ok((
        input,
        ApplicationExtension {
            identifier,
            authentication_code,
            data: data.to_vec(),
        },
    ))
}

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <gif_file>", args[0]);
        std::process::exit(1);
    }

    let path = Path::new(&args[1]);
    let data = match read(path) {
        Ok(data) => data,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            std::process::exit(1);
        }
    };

    match gif_header(&data) {
        Ok((_, header)) => println!("GIF Header: {:?}", header),
        Err(e) => {
            eprintln!("Error parsing GIF header: {:?}", e);
            std::process::exit(1);
        }
    }
}
