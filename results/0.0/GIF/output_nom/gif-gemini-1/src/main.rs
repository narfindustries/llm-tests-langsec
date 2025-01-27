use std::env;
use std::fs;
use std::error::Error;
use nom::{
    IResult,
    bytes::complete::{take, tag},
    number::complete::{be_u16, be_u32, le_u16},
    multi::count,
    sequence::tuple,
    combinator::{map, map_res, opt, all_consuming},
};

#[derive(Debug)]
struct GifHeader {
    signature: [u8; 6],
    version: [u8; 3],
}

#[derive(Debug)]
struct GifScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: u8,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

#[derive(Debug)]
struct GifImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: u8,
    local_color_table_flag: bool,
    lct_size: Option<u8>,
    lct: Option<Vec<u8>>,
}


#[derive(Debug)]
struct GifGraphicControlExtension {
    packed_fields: u8,
    delay_time: u16,
    transparent_color_index: u8,
    terminator: u8,
}

#[derive(Debug)]
struct GifCommentExtension {
    comment: Vec<u8>,
}

#[derive(Debug)]
struct GifApplicationExtension {
    application_identifier: [u8; 8],
    authentication_code: [u8; 3],
    data: Vec<u8>,
}

#[derive(Debug)]
enum GifExtension {
    GraphicControl(GifGraphicControlExtension),
    Comment(GifCommentExtension),
    Application(GifApplicationExtension),
}


#[derive(Debug)]
struct Gif {
    header: GifHeader,
    screen_descriptor: GifScreenDescriptor,
    global_color_table: Option<Vec<u8>>,
    images: Vec<GifImageDescriptor>,
    extensions: Vec<GifExtension>,
    trailer: u8,
}

fn gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    map(tuple((tag(b"GIF87a"), tag(b"GIF89a"))), |(signature, _)| GifHeader {
        signature: [signature[0], signature[1], signature[2], signature[3], signature[4], signature[5]],
        version: [signature[0], signature[1], signature[2]],
    })(input)
}

fn gif_screen_descriptor(input: &[u8]) -> IResult<&[u8], GifScreenDescriptor> {
    map(tuple((be_u16, be_u16, u8, u8, u8)), |(width, height, packed_fields, background_color_index, pixel_aspect_ratio)| GifScreenDescriptor {
        width,
        height,
        packed_fields,
        background_color_index,
        pixel_aspect_ratio,
    })(input)
}

fn gif_global_color_table(input: &[u8], size: u8) -> IResult<&[u8], Vec<u8>> {
    let num_entries = (1 << (size + 1)) as usize;
    count(take(3), num_entries)(input)
}

fn gif_image_descriptor(input: &[u8]) -> IResult<&[u8], GifImageDescriptor> {
    map(tuple((be_u16, be_u16, be_u16, be_u16, u8)), |(left, top, width, height, packed_fields)| {
        let local_color_table_flag = (packed_fields >> 7) & 1 != 0;
        let lct_size = if local_color_table_flag { Some((packed_fields >> 4) & 7) } else { None };
        GifImageDescriptor {
            left,
            top,
            width,
            height,
            packed_fields,
            local_color_table_flag,
            lct_size,
            lct: None,
        }
    })(input)
}

fn gif_graphic_control_extension(input: &[u8]) -> IResult<&[u8], GifGraphicControlExtension> {
    map(tuple((u8, le_u16, u8, u8)), |(packed_fields, delay_time, transparent_color_index, terminator)| GifGraphicControlExtension {
        packed_fields,
        delay_time,
        transparent_color_index,
        terminator,
    })(input)
}

fn gif_comment_extension(input: &[u8]) -> IResult<&[u8], GifCommentExtension> {
    map_res(many0(take(255)), |chunks| {
        let mut comment = Vec::new();
        for chunk in chunks {
            comment.extend_from_slice(chunk);
        }
        Ok(GifCommentExtension { comment })
    })(input)
}

fn gif_application_extension(input: &[u8]) -> IResult<&[u8], GifApplicationExtension> {
    map(tuple((tag(b"NETSCAPE2.0"), take(3), many0(take(255)))), |(application_identifier, authentication_code, data)| GifApplicationExtension {
        application_identifier: [application_identifier[0], application_identifier[1], application_identifier[2], application_identifier[3], application_identifier[4], application_identifier[5], application_identifier[6], application_identifier[7]],
        authentication_code: [authentication_code[0], authentication_code[1], authentication_code[2]],
        data: data.concat(),
    })(input)
}

fn gif_extension(input: &[u8]) -> IResult<&[u8], GifExtension> {
    // Implement other extension types as needed
    Ok(("", GifExtension::Comment(GifCommentExtension { comment: vec![] })))
}

fn many0<I, O, E, F>(f: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: Clone + std::fmt::Debug,
    F: FnMut(I) -> IResult<I, O, E>,
    E: nom::error::ParseError<I>,
{
    nom::multi::many0(f)
}

fn main() -> Result<(), Box<dyn Error>> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        return Ok(());
    }

    let filename = &args[1];
    let data = fs::read(filename)?;

    let result = all_consuming(gif_header)(&data);

    match result {
        Ok((_, header)) => println!("GIF Header: {:?}", header),
        Err(e) => eprintln!("Error parsing GIF header: {}", e),
    }

    Ok(())
}
