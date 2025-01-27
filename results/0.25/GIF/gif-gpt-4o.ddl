// GIF File Format Specification in Daedalus

// GIF Header
struct GifHeader {
    signature: string[3]; // "GIF"
    version: string[3]; // "87a" or "89a"
}

// Logical Screen Descriptor
struct LogicalScreenDescriptor {
    canvas_width: u2;
    canvas_height: u2;
    packed_fields: u1;
    background_color_index: u1;
    pixel_aspect_ratio: u1;
}

// Global Color Table
struct ColorTableEntry {
    red: u1;
    green: u1;
    blue: u1;
}

struct GlobalColorTable {
    colors: ColorTableEntry[];
}

// Image Descriptor
struct ImageDescriptor {
    image_separator: u1; // 0x2C
    image_left_position: u2;
    image_top_position: u2;
    image_width: u2;
    image_height: u2;
    packed_fields: u1;
}

// Local Color Table
struct LocalColorTable {
    colors: ColorTableEntry[];
}

// Table Based Image Data
struct ImageData {
    lzw_minimum_code_size: u1;
    image_data_blocks: Block[];
}

struct Block {
    block_size: u1;
    block_data: u1[block_size];
}

// Graphics Control Extension
struct GraphicsControlExtension {
    extension_introducer: u1; // 0x21
    graphic_control_label: u1; // 0xF9
    block_size: u1; // 0x04
    packed_fields: u1;
    delay_time: u2;
    transparent_color_index: u1;
    block_terminator: u1; // 0x00
}

// Comment Extension
struct CommentExtension {
    extension_introducer: u1; // 0x21
    comment_label: u1; // 0xFE
    comment_data: Block[];
}

// Plain Text Extension
struct PlainTextExtension {
    extension_introducer: u1; // 0x21
    plain_text_label: u1; // 0x01
    block_size: u1; // 0x0C
    text_grid_left_position: u2;
    text_grid_top_position: u2;
    text_grid_width: u2;
    text_grid_height: u2;
    character_cell_width: u1;
    character_cell_height: u1;
    text_foreground_color_index: u1;
    text_background_color_index: u1;
    plain_text_data: Block[];
}

// Application Extension
struct ApplicationExtension {
    extension_introducer: u1; // 0x21
    application_label: u1; // 0xFF
    block_size: u1; // 0x0B
    application_identifier: string[8];
    application_authentication_code: string[3];
    application_data: Block[];
}

// Trailer
struct Trailer {
    trailer: u1; // 0x3B
}

// GIF File
struct GifFile {
    header: GifHeader;
    logical_screen_descriptor: LogicalScreenDescriptor;
    global_color_table: GlobalColorTable if logical_screen_descriptor.packed_fields & 0x80 != 0;
    blocks: (GraphicsControlExtension | CommentExtension | PlainTextExtension | ApplicationExtension | ImageDescriptor | ImageData)[];
    trailer: Trailer;
}