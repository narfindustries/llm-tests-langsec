parsers GIF;

type Version = enum {
    | "87a"
    | "89a"
};

type ColorResolution = 1..8;
type ColorTableSize = 0..7;

struct Header {
    signature: bytes(3) = b"GIF",
    version: Version
};

struct PackedScreenDescriptor {
    global_color_table_flag: bool,
    color_resolution: ColorResolution,
    sort_flag: bool,
    global_color_table_size: ColorTableSize
};

struct LogicalScreenDescriptor {
    width: u16,
    height: u16,
    packed_fields: PackedScreenDescriptor,
    background_color_index: u8,
    pixel_aspect_ratio: u8
};

struct RGBColor {
    red: u8,
    green: u8,
    blue: u8
};

struct GlobalColorTable {
    colors: RGBColor[1 << (global_color_table_size + 1)]
};

struct PackedImageDescriptor {
    local_color_table_flag: bool,
    interlace_flag: bool,
    sort_flag: bool,
    local_color_table_size: ColorTableSize
};

struct ImageDescriptor {
    separator: u8 = 0x2C,
    left_position: u16,
    top_position: u16,
    width: u16,
    height: u16,
    packed_fields: PackedImageDescriptor
};

struct LocalColorTable {
    colors: RGBColor[1 << (local_color_table_size + 1)]
};

struct GraphicControlExtension {
    block_identifier: u8 = 0x21,
    graphic_control_label: u8 = 0xF9,
    block_size: u8 = 4,
    packed_fields: {
        disposal_method: 0..7,
        user_input_flag: bool,
        transparent_color_flag: bool
    },
    delay_time: u16,
    transparent_color_index: u8,
    block_terminator: u8 = 0x00
};

struct CommentExtension {
    block_identifier: u8 = 0x21,
    comment_label: u8 = 0xFE,
    block_size: u8,
    comment_data: bytes(block_size),
    block_terminator: u8 = 0x00
};

struct ApplicationExtension {
    block_identifier: u8 = 0x21,
    application_label: u8 = 0xFF,
    block_size: u8 = 0x0B,
    application_identifier: bytes(8),
    authentication_code: bytes(3),
    application_data: bytes,
    block_terminator: u8 = 0x00
};

struct GIFFile {
    header: Header,
    logical_screen_descriptor: LogicalScreenDescriptor,
    global_color_table?: GlobalColorTable,
    image_descriptor: ImageDescriptor,
    local_color_table?: LocalColorTable,
    image_data: bytes,
    extensions?: (
        | GraphicControlExtension
        | CommentExtension
        | ApplicationExtension
    )*,
    trailer: u8 = 0x3B
};

parse(input: bytes): GIFFile;