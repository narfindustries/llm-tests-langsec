use nom::{
    bytes::complete::{tag, take},
    combinator::{map, map_res},
    number::complete::{be_u16, be_u8},
    IResult,
};
use std::env;
use std::fs::File;
use std::io::Read;

#[derive(Debug, PartialEq)]
enum ColorTableFlag {
    Present,
    NotPresent,
}

#[derive(Debug, PartialEq)]
enum SortFlag {
    Sorted,
    NotSorted,
}

#[derive(Debug, PartialEq)]
enum InterlaceFlag {
    Interlaced,
    NotInterlaced,
}

#[derive(Debug, PartialEq)]
struct PackedFields {
    global_color_table_flag: ColorTableFlag,
    color_resolution: u8,
    sort_flag: SortFlag,
    size_of_global_color_table: u8,
}

impl PackedFields {
    fn new(value: u8) -> Self {
        PackedFields {
            global_color_table_flag: if (value & 0x80) != 0 {
                ColorTableFlag::Present
            } else {
                ColorTableFlag::NotPresent
            },
            color_resolution: (value & 0x70) >> 4,
            sort_flag: if (value & 0x08) != 0 {
                SortFlag::Sorted
            } else {
                SortFlag::NotSorted
            },
            size_of_global_color_table: (value & 0x07),
        }
    }
}

#[derive(Debug, PartialEq)]
struct Color {
    red: u8,
    green: u8,
    blue: u8,
}

fn color(input: &[u8]) -> IResult<&[u8], Color> {
    let (input, red) = take(1u8)(input)?;
    let (input, green) = take(1u8)(input)?;
    let (input, blue) = take(1u8)(input)?;
    Ok((input, Color { red: red[0], green: green[0], blue: blue[0] }))
}

#[derive(Debug, PartialEq)]
struct GlobalColorTable {
    colors: Vec<Color>,
}

fn global_color_table(input: &[u8], size: u8) -> IResult<&[u8], GlobalColorTable> {
    let (input, colors) = take((2usize.pow((size + 1) as u32)) * 3)(input)?;
    let mut colors_vec = Vec::new();
    for chunk in colors.chunks(3) {
        let color = Color {
            red: chunk[0],
            green: chunk[1],
            blue: chunk[2],
        };
        colors_vec.push(color);
    }
    Ok((input, GlobalColorTable { colors: colors_vec }))
}

#[derive(Debug, PartialEq)]
struct ImageDescriptor {
    image_separator: u8,
    image_left_position: u16,
    image_top_position: u16,
    image_width: u16,
    image_height: u16,
    packed_fields: PackedFields,
}

fn image_descriptor(input: &[u8]) -> IResult<&[u8], ImageDescriptor> {
    let (input, image_separator) = tag(&[0x2C])(input)?;
    let (input, image_left_position) = be_u16(input)?;
    let (input, image_top_position) = be_u16(input)?;
    let (input, image_width) = be_u16(input)?;
    let (input, image_height) = be_u16(input)?;
    let (input, packed_fields_value) = be_u8(input)?;
    let packed_fields = PackedFields::new(packed_fields_value);
    Ok((input, ImageDescriptor {
        image_separator: image_separator[0],
        image_left_position,
        image_top_position,
        image_width,
        image_height,
        packed_fields,
    }))
}

#[derive(Debug, PartialEq)]
struct LocalColorTable {
    colors: Vec<Color>,
}

fn local_color_table(input: &[u8], size: u8) -> IResult<&[u8], LocalColorTable> {
    let (input, colors) = take((2usize.pow((size + 1) as u32)) * 3)(input)?;
    let mut colors_vec = Vec::new();
    for chunk in colors.chunks(3) {
        let color = Color {
            red: chunk[0],
            green: chunk[1],
            blue: chunk[2],
        };
        colors_vec.push(color);
    }
    Ok((input, LocalColorTable { colors: colors_vec }))
}

#[derive(Debug, PartialEq)]
struct Gif {
    signature: [u8; 3],
    version: [u8; 3],
    logical_screen_width: u16,
    logical_screen_height: u16,
    packed_fields: PackedFields,
    background_color_index: u8,
    pixel_aspect_ratio: u8,
    global_color_table: Option<GlobalColorTable>,
    image_descriptor: ImageDescriptor,
    local_color_table: Option<LocalColorTable>,
}

fn gif(input: &[u8]) -> IResult<&[u8], Gif> {
    let (input, signature) = take(3u8)(input)?;
    let (input, version) = take(3u8)(input)?;
    let (input, logical_screen_width) = be_u16(input)?;
    let (input, logical_screen_height) = be_u16(input)?;
    let (input, packed_fields_value) = be_u8(input)?;
    let packed_fields = PackedFields::new(packed_fields_value);
    let (input, background_color_index) = be_u8(input)?;
    let (input, pixel_aspect_ratio) = be_u8(input)?;
    let global_color_table = if packed_fields.global_color_table_flag == ColorTableFlag::Present {
        let (input, global_color_table) = global_color_table(input, packed_fields.size_of_global_color_table)?;
        Some(global_color_table)
    } else {
        let (input, _) = take(0u8)(input)?;
        None
    };
    let (input, image_descriptor) = image_descriptor(input)?;
    let local_color_table = if image_descriptor.packed_fields.global_color_table_flag == ColorTableFlag::Present {
        let (input, local_color_table) = local_color_table(input, image_descriptor.packed_fields.size_of_global_color_table)?;
        Some(local_color_table)
    } else {
        let (input, _) = take(0u8)(input)?;
        None
    };
    Ok((input, Gif {
        signature: signature.try_into().unwrap(),
        version: version.try_into().unwrap(),
        logical_screen_width,
        logical_screen_height,
        packed_fields,
        background_color_index,
        pixel_aspect_ratio,
        global_color_table,
        image_descriptor,
        local_color_table,
    }))
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_path = &args[1];
    let mut file = File::open(file_path).unwrap();
    let mut buffer = Vec::new();
    file.read_to_end(&mut buffer).unwrap();
    let result = gif(&buffer);
    match result {
        Ok((remaining, gif)) => {
            println!("GIF: {:?}", gif);
            println!("Remaining: {:?}", remaining);
        }
        Err(err) => {
            println!("Error: {:?}", err);
        }
    }
}