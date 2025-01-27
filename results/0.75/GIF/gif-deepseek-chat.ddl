struct GIFHeader {
    signature: magic b"GIF";
    version: bytes[3];
}

struct LogicalScreenDescriptor {
    width: u16;
    height: u16;
    packed_fields: u8;
    background_color_index: u8;
    pixel_aspect_ratio: u8;
}

struct ColorTableEntry {
    red: u8;
    green: u8;
    blue: u8;
}

struct ColorTable {
    entries: ColorTableEntry[2^(packed_fields & 0x07 + 1)];
}

struct ImageDescriptor {
    separator: u8;
    left: u16;
    top: u16;
    width: u16;
    height: u16;
    packed_fields: u8;
}

struct LocalColorTable {
    entries: ColorTableEntry[2^(packed_fields & 0x07 + 1)];
}

struct ImageData {
    lzw_minimum_code_size: u8;
    sub_blocks: SubBlock[];
}

struct SubBlock {
    block_size: u8;
    data: bytes[block_size];
}

struct GraphicControlExtension {
    extension_introducer: u8;
    graphic_control_label: u8;
    block_size: u8;
    packed_fields: u8;
    delay_time: u16;
    transparent_color_index: u8;
    terminator: u8;
}

struct PlainTextExtension {
    extension_introducer: u8;
    plain_text_label: u8;
    block_size: u8;
    text_grid_left_position: u16;
    text_grid_top_position: u16;
    text_grid_width: u16;
    text_grid_height: u16;
    character_cell_width: u8;
    character_cell_height: u8;
    text_foreground_color_index: u8;
    text_background_color_index: u8;
    plain_text_data: SubBlock[];
}

struct ApplicationExtension {
    extension_introducer: u8;
    application_label: u8;
    block_size: u8;
    application_identifier: bytes[8];
    application_authentication_code: bytes[3];
    application_data: SubBlock[];
}

struct CommentExtension {
    extension_introducer: u8;
    comment_label: u8;
    comment_data: SubBlock[];
}

struct GIFTrailer {
    trailer: u8;
}

struct GIF {
    header: GIFHeader;
    logical_screen_descriptor: LogicalScreenDescriptor;
    global_color_table: ColorTable if logical_screen_descriptor.packed_fields & 0x80;
    blocks: Block[];
    trailer: GIFTrailer;
}

enum Block {
    image_descriptor: ImageDescriptor {
        local_color_table: LocalColorTable if packed_fields & 0x80;
        image_data: ImageData;
    };
    graphic_control_extension: GraphicControlExtension;
    plain_text_extension: PlainTextExtension;
    application_extension: ApplicationExtension;
    comment_extension: CommentExtension;
}