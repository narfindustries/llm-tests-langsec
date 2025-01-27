use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::take_till,
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::{
    env, fs,
    io::{self, Read},
    str,
};

#[derive(Debug, PartialEq)]
struct GifHeader {
    signature: [u8; 3],
    version: [u8; 3],
}

fn gif_header(input: &[u8]) -> IResult<&[u8], GifHeader> {
    let (input, signature) = take(3usize)(input)?;
    let (input, version) = take(3usize)(input)?;
    Ok((
        input,
        GifHeader {
            signature: signature.try_into().unwrap(),
            version: version.try_into().unwrap(),
        },
    ))
}

#[derive(Debug, PartialEq)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    flags: u8,
    bg_color_index: u8,
    pixel_aspect_ratio: u8,
}

fn logical_screen_descriptor(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
    let (input, width) = be_u16(input)?;
    let (input, height) = be_u16(input)?;
    let (input, flags) = be_u8(input)?;
    let (input, bg_color_index) = be_u8(input)?;
    let (input, pixel_aspect_ratio) = be_u8(input)?;
    Ok((
        input,
        LogicalScreenDescriptor {
            width,
            height,
            flags,
            bg_color_index,
            pixel_aspect_ratio,
        },
    ))
}

#[derive(Debug, PartialEq)]
struct GlobalColorTable {
    colors: Vec<[u8; 3]>,
}

fn global_color_table(input: &[u8], size: usize) -> IResult<&[u8], GlobalColorTable> {
    let (input, colors) = take(size)(input)?;
    let mut colors_vec = Vec::new();
    for chunk in colors.chunks(3) {
        colors_vec.push(chunk.try_into().unwrap());
    }
    Ok((
        input,
        GlobalColorTable {
            colors: colors_vec,
        },
    ))
}

#[derive(Debug, PartialEq)]
enum Block {
    ImageDescriptor(ImageDescriptor),
    Extension Extension,
    Trailer,
}

#[derive(Debug, PartialEq)]
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    flags: u8,
}

fn image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, left) = be_u16(input)?;
    let (input, top) = be_u16(input)?;
    let (input, width) = be_u16(input)?;
    let (input, height) = be_u16(input)?;
    let (input, flags) = be_u8(input)?;
    Ok((
        input,
        ImageDescriptor {
            left,
            top,
            width,
            height,
            flags,
        },
    ))
}

#[derive(Debug, PartialEq)]
enum Extension {
    GraphicControl(GraphicControl),
    Comment(Comment),
    PlainText(PlainText),
    Application(Application),
}

#[derive(Debug, PartialEq)]
struct GraphicControl {
    label: u8,
    block_size: u8,
    flags: u8,
    delay_time: u16,
    transparent_color_index: u8,
}

fn graphic_control(input: &[u8]) -> IResult<&[u8], GraphicControl> {
    let (input, label) = be_u8(input)?;
    let (input, block_size) = be_u8(input)?;
    let (input, flags) = be_u8(input)?;
    let (input, delay_time) = be_u16(input)?;
    let (input, transparent_color_index) = be_u8(input)?;
    Ok((
        input,
        GraphicControl {
            label,
            block_size,
            flags,
            delay_time,
            transparent_color_index,
        },
    ))
}

#[derive(Debug, PartialEq)]
struct Comment {
    text: String,
}

fn comment(input: &[u8]) -> IResult<&[u8], Comment> {
    let (input, _) = tag(&[0x21, 0xfe])(input)?;
    let (input, block_size) = be_u8(input)?;
    let (input, text) = take_till(|c| c == 0)(input)?;
    Ok((
        input,
        Comment {
            text: str::from_utf8(text).unwrap().to_string(),
        },
    ))
}

#[derive(Debug, PartialEq)]
struct PlainText {
    text: String,
}

fn plain_text(input: &[u8]) -> IResult<&[u8], PlainText> {
    let (input, _) = tag(&[0x21, 0x01])(input)?;
    let (input, block_size) = be_u8(input)?;
    let (input, text) = take_till(|c| c == 0)(input)?;
    Ok((
        input,
        PlainText {
            text: str::from_utf8(text).unwrap().to_string(),
        },
    ))
}

#[derive(Debug, PartialEq)]
struct Application {
    data: Vec<u8>,
}

fn application(input: &[u8]) -> IResult<&[u8], Application> {
    let (input, _) = tag(&[0x21, 0xff])(input)?;
    let (input, block_size) = be_u8(input)?;
    let (input, data) = take(block_size as usize)(input)?;
    Ok((
        input,
        Application {
            data: data.to_vec(),
        },
    ))
}

fn block(input: &[u8]) -> IResult<&[u8], Block> {
    let (input, identifier) = be_u8(input)?;
    match identifier {
        0x2c => {
            let (input, image_descriptor) = image_descriptor(input)?;
            Ok((input, Block::ImageDescriptor(image_descriptor)))
        }
        0x21 => {
            let (input, extension_label) = be_u8(input)?;
            match extension_label {
                0xf9 => {
                    let (input, graphic_control) = graphic_control(input)?;
                    Ok((input, Block::Extension(Extension::GraphicControl(graphic_control))))
                }
                0xfe => {
                    let (input, comment) = comment(input)?;
                    Ok((input, Block::Extension(Extension::Comment(comment))))
                }
                0x01 => {
                    let (input, plain_text) = plain_text(input)?;
                    Ok((input, Block::Extension(Extension::PlainText(plain_text))))
                }
                0xff => {
                    let (input, application) = application(input)?;
                    Ok((input, Block::Extension(Extension::Application(application))))
                }
                _ => unreachable!(),
            }
        }
        0x3b => Ok((input, Block::Trailer)),
        _ => unreachable!(),
    }
}

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return Ok(());
    }

    let mut file = fs::File::open(&args[1])?;
    let mut data = Vec::new();
    file.read_to_end(&mut data)?;

    let (input, header) = gif_header(&data).unwrap();
    println!("Header: {:?}", header);

    let (input, logical_screen_descriptor) = logical_screen_descriptor(input).unwrap();
    println!("Logical Screen Descriptor: {:?}", logical_screen_descriptor);

    let (input, global_color_table) = global_color_table(input, 768).unwrap();
    println!("Global Color Table: {:?}", global_color_table);

    let mut blocks = Vec::new();
    while !input.is_empty() {
        let (input, block) = block(input).unwrap();
        blocks.push(block);
    }

    for block in blocks {
        match block {
            Block::ImageDescriptor(image_descriptor) => {
                println!("Image Descriptor: {:?}", image_descriptor);
            }
            Block::Extension(extension) => match extension {
                Extension::GraphicControl(graphic_control) => {
                    println!("Graphic Control: {:?}", graphic_control);
                }
                Extension::Comment(comment) => {
                    println!("Comment: {:?}", comment);
                }
                Extension::PlainText(plain_text) => {
                    println!("Plain Text: {:?}", plain_text);
                }
                Extension::Application(application) => {
                    println!("Application: {:?}", application);
                }
            },
            Block::Trailer => {
                println!("Trailer");
            }
        }
    }

    Ok(())
}