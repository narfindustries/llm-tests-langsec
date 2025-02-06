use nom::{
    bytes::complete::{tag, take},
    combinator::{map, opt},
    multi::{many_till},
    number::complete::{be_u16, be_u8},
    sequence::{preceded, tuple},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::{BufReader, Read};
use std::path::Path;

#[derive(Debug, PartialEq)]
enum DisposalMethod {
    NoDisposal,
    DoNotDispose,
    RestoreToBackground,
    RestoreToPrevious,
    NoOperation,
    RestoreToTransparent,
}

#[derive(Debug, PartialEq)]
struct PackedFields {
    global_color_table_flag: bool,
    color_resolution: u8,
    sort_flag: bool,
    size_of_global_color_table: u8,
}

impl PackedFields {
    fn parse(input: &[u8]) -> IResult<&[u8], PackedFields> {
        map(be_u8, |x| PackedFields {
            global_color_table_flag: (x & 0x80) != 0,
            color_resolution: (x & 0x70) >> 4,
            sort_flag: (x & 0x08) != 0,
            size_of_global_color_table: x & 0x07,
        })(input)
    }
}

#[derive(Debug, PartialEq)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
}

impl Color {
    fn parse(input: &[u8]) -> IResult<&[u8], Color> {
        map(tuple((be_u8, be_u8, be_u8)), |(red, green, blue)| Color {
            red,
            green,
            blue,
        })(input)
    }
}

#[derive(Debug, PartialEq)]
struct GlobalColorTable {
    colors: Vec<Color>,
}

impl GlobalColorTable {
    fn parse(input: &[u8], size: usize) -> IResult<&[u8], GlobalColorTable> {
        map(take(size * 3), |x: &[u8]| {
            let mut colors = Vec::new();
            for chunk in x.chunks(3) {
                let (_, color) = Color::parse(chunk)?;
                colors.push(color);
            }
            Ok((b"", GlobalColorTable { colors }))
        })(input)
    }
}

#[derive(Debug, PartialEq)]
struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: PackedFields,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
}

impl LogicalScreenDescriptor {
    fn parse(input: &[u8]) -> IResult<&[u8], LogicalScreenDescriptor> {
        map(
            tuple((be_u16, be_u16, PackedFields::parse, be_u8, be_u8)),
            |(width, height, packed_fields, background_color_index, pixel_aspect_ratio)| {
                LogicalScreenDescriptor {
                    width,
                    height,
                    packed_fields,
                    background_color_index,
                    pixel_aspect_ratio,
                }
            },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct GraphicControlExtension {
    disposal_method: DisposalMethod,
    user_input_flag: bool,
    transparent_color_flag: bool,
    delay_time: u16,
    transparent_color_index: Option<u8>,
}

impl GraphicControlExtension {
    fn parse(input: &[u8]) -> IResult<&[u8], GraphicControlExtension> {
        map(
            tuple((
                be_u8,
                be_u8,
                be_u16,
                opt(be_u8),
            )),
            |(disposal_method, flags, delay_time, transparent_color_index)| {
                GraphicControlExtension {
                    disposal_method: match disposal_method {
                        0 => DisposalMethod::NoDisposal,
                        1 => DisposalMethod::DoNotDispose,
                        2 => DisposalMethod::RestoreToBackground,
                        3 => DisposalMethod::RestoreToPrevious,
                        4 => DisposalMethod::NoOperation,
                        5 => DisposalMethod::RestoreToTransparent,
                        _ => unreachable!(),
                    },
                    user_input_flag: (flags & 0x02) != 0,
                    transparent_color_flag: (flags & 0x01) != 0,
                    delay_time,
                    transparent_color_index,
                }
            },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct ImageDescriptor {
    left: u16,
    top: u16,
    width: u16,
    height: u16,
    packed_fields: PackedFields,
}

impl ImageDescriptor {
    fn parse(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
        map(
            tuple((be_u16, be_u16, be_u16, be_u16, PackedFields::parse)),
            |(left, top, width, height, packed_fields)| ImageDescriptor {
                left,
                top,
                width,
                height,
                packed_fields,
            },
        )(input)
    }
}

#[derive(Debug, PartialEq)]
struct Gif {
    header: [u8; 6],
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table: Option<GlobalColorTable>,
    graphic_control_extensions: Vec<GraphicControlExtension>,
    image_descriptors: Vec<ImageDescriptor>,
}

impl Gif {
    fn parse(input: &[u8]) -> IResult<&[u8], Gif> {
        map(
            tuple((
                take(6),
                LogicalScreenDescriptor::parse,
                opt(|i| {
                    let (i, packed_fields) = PackedFields::parse(i)?;
                    let size = 3 * (2usize.pow((packed_fields.size_of_global_color_table + 1) as u32));
                    GlobalColorTable::parse(i, size)
                }),
                many_till(
                    preceded(tag(&[0x21]), tuple((be_u8, opt(GraphicControlExtension::parse)))),
                    tag(&[0x3b]),
                ),
                many_till(
                    preceded(tag(&[0x2c]), ImageDescriptor::parse),
                    tag(&[0x3b]),
                ),
            )),
            |(header, logical_screen_descriptor, global_color_table, graphic_control_extensions, image_descriptors)| {
                let mut graphic_control_extensions = graphic_control_extensions
                    .into_iter()
                    .filter_map(|x| x.1)
                    .collect::<Vec<GraphicControlExtension>>();
                let mut image_descriptors = image_descriptors
                    .into_iter()
                    .map(|x| x.1)
                    .collect::<Vec<ImageDescriptor>>();
                Gif {
                    header: {
                        let mut header_array: [u8; 6] = [0; 6];
                        header_array.copy_from_slice(&header);
                        header_array
                    },
                    logical_screen_descriptor,
                    global_color_table,
                    graphic_control_extensions,
                    image_descriptors,
                }
            },
        )(input)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        println!("Usage: {} <input_file>", args[0]);
        return;
    }
    let path = Path::new(&args[1]);
    let file = File::open(path).unwrap();
    let mut reader = BufReader::new(file);
    let mut input = Vec::new();
    reader.read_to_end(&mut input).unwrap();
    let result = Gif::parse(&input);
    match result {
        Ok((_, gif)) => println!("{:?}", gif),
        Err(err) => println!("Error: {}", err),
    }
}