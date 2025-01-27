use std::env;
use std::fs;
use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res, opt, recognize},
    error::ErrorKind,
    multi::count,
    number::complete::{be_u16, be_u32, be_u8},
    sequence::{tuple, preceded},
    IResult,
};

#[derive(Debug)]
enum GifColorTable {
    Global(Vec<[u8; 3]>),
    Local(Vec<[u8; 3]>),
}


#[derive(Debug)]
struct GifHeader {
    signature: [u8; 6],
    version: [u8; 3],
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<GifColorTable>,
    blocks: Vec<GifBlock>,
}

#[derive(Debug)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    flags: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
enum GifBlock {
    ImageDescriptor(GifImageDescriptor),
    Extension(GifExtension),
    Trailer(u8),

}

#[derive(Debug)]
struct GifImageDescriptor {
    image_left_position: u16,
    image_top_position: u16,
    image_width: u16,
    image_height: u16,
    flags: u8,
    local_color_table: Option<GifColorTable>,
    image_data: Vec<u8>,
}

#[derive(Debug)]
struct GifExtension {
    extension_type: u8,
    data: Vec<u8>,
}

fn parse_color_table(count: usize) -> impl Fn(&[u8]) -> IResult<&[u8], Vec<[u8; 3]>> {
    move |i| {
        let (i, entries) = count(take(3usize), count)(i)?;
        Ok((i, entries))
    }
}


fn gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = tag(b"GIF87a")(input)?;
    let (input, version) = take(3usize)(input)?;
    let (input, logical_screen_descriptor) = logical_screen_descriptor(input)?;

    let (input, global_color_table) = opt(preceded(
            tag(b"!"),
            map(|i| {
              let (i, count) = be_u8(i)?;
              let count = (count as usize) * 3;
              let (i, colors) = parse_color_table(count)(i)?;
              Ok((i, GifColorTable::Global(colors)))
            }
            )(input)
    ))(input)?;

    let (input, blocks) = many0(gif_block)(input)?;


    Ok((
        input,
        GifHeader {
            signature: *signature.try_into().unwrap(),
            version: *version.try_into().unwrap(),
            logical_screen_descriptor,
            global_color_table,
            blocks,
        },
    ))
}

fn logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, (width, height, flags, background_color_index, pixel_aspect_ratio)) = tuple((
        be_u16,
        be_u16,
        be_u8,
        be_u8,
        be_u8,
    ))(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            flags,
            background_color_index,
            pixel_aspect_ratio,
        },
    ))
}

fn gif_block(input: &[u8]) -> IResult<&[u8], GifBlock> {
    let (input, block_type) = be_u8(input)?;

    match block_type {
        0x2C => {
          let (input, image_descriptor) = gif_image_descriptor(input)?;
          Ok((input, GifBlock::ImageDescriptor(image_descriptor)))
        },
        0x21 => {
            let (input, extension) = gif_extension(input)?;
            Ok((input, GifBlock::Extension(extension)))
        },
        0x3B => Ok((input, GifBlock::Trailer(block_type))),
        _ => Err(nom::Err::Error((input, ErrorKind::Tag)))
    }
}


fn gif_image_descriptor(input: &[u8]) -> IResult<&[u8], GifImageDescriptor> {
    let (input, (image_left_position, image_top_position, image_width, image_height, flags)) = tuple((
        be_u16,
        be_u16,
        be_u16,
        be_u16,
        be_u8,
    ))(input)?;
    let (input, local_color_table) = opt(map(|i| {
        let (i, count) = be_u8(i)?;
        let count = (count as usize) * 3;
        let (i, colors) = parse_color_table(count)(i)?;
        Ok((i, GifColorTable::Local(colors)))
    })(input))?;

    let (input, image_data) = gif_data_blocks(input)?;
    Ok((
        input,
        GifImageDescriptor {
            image_left_position,
            image_top_position,
            image_width,
            image_height,
            flags,
            local_color_table,
            image_data,
        },
    ))
}


fn gif_extension(input: &[u8]) -> IResult<&[u8], GifExtension> {
    let (input, extension_type) = be_u8(input)?;
    let (input, data) = gif_data_blocks(input)?;
    Ok((
        input,
        GifExtension {
            extension_type,
            data,
        },
    ))
}


fn gif_data_blocks(input: &[u8]) -> IResult<&[u8], Vec<u8>> {
    let mut data = Vec::new();
    let mut input = input;
    loop {
        let (i, block_size) = be_u8(input)?;
        if block_size == 0 {
            break;
        }
        let (i, block_data) = take(block_size as usize)(i)?;
        data.extend_from_slice(block_data);
        input = i;
    }
    Ok((input, data))
}

fn many0<I, O, E, F>(f: F) -> impl Fn(I) -> IResult<I, Vec<O>, E>
where
    I: Clone + nom::InputTakeAtPosition,
    F: Fn(I) -> IResult<I, O, E>,
    E: nom::error::ParseError<I>,
{
    move |i| {
        let mut acc = Vec::new();
        let mut input = i;
        loop {
            match f(input.clone()) {
                Ok((i, o)) => {
                    acc.push(o);
                    input = i;
                }
                Err(nom::Err::Error(e)) => {
                    return Err(nom::Err::Error(e));
                }
                Err(nom::Err::Incomplete(_)) => {
                    return Err(nom::Err::Incomplete(_));
                }
                Err(nom::Err::Failure(e)) => {
                    return Err(nom::Err::Failure(e));
                }
            }
            if input.is_empty() {
                break;
            }
        }
        Ok((input, acc))
    }
}


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: gif_parser <filename>");
        return;
    }

    let filename = &args[1];
    let data = match fs::read(filename) {
        Ok(data) => data,
        Err(e) => {
            println!("Error reading file: {}", e);
            return;
        }
    };

    match gif_header(&data) {
        Ok((_, header)) => println!("GIF Header: {:?}", header),
        Err(e) => println!("Error parsing GIF: {:?}", e),
    }
}
