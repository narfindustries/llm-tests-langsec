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

struct ColorTable {
    entries: u8[][3];
}

struct ImageDescriptor {
    separator: u8;
    left_position: u16;
    top_position: u16;
    width: u16;
    height: u16;
    packed_fields: u8;
}

struct ImageData {
    lzw_minimum_code_size: u8;
    blocks: Block[];
}

struct Block {
    block_size: u8;
    data: bytes[block_size];
}

struct GIFTrailer {
    trailer: u8;
}

struct GIF {
    header: GIFHeader;
    logical_screen_descriptor: LogicalScreenDescriptor;
    global_color_table: ColorTable if logical_screen_descriptor.packed_fields & 0x80;
    image_descriptor: ImageDescriptor;
    local_color_table: ColorTable if image_descriptor.packed_fields & 0x80;
    image_data: ImageData;
    trailer: GIFTrailer;
}