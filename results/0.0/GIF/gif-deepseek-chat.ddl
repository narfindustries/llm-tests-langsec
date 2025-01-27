struct GIFHeader {
    signature: [3]byte; // "GIF"
    version: [3]byte;   // "87a" or "89a"
}

struct LogicalScreenDescriptor {
    width: u16;
    height: u16;
    packed_fields: byte;
    background_color_index: byte;
    pixel_aspect_ratio: byte;
}

struct ColorTable {
    entries: []RGB {
        count: (logical_screen_descriptor.packed_fields & 0x07) + 1;
    };
}

struct RGB {
    red: byte;
    green: byte;
    blue: byte;
}

struct ImageDescriptor {
    separator: byte; // 0x2C
    left: u16;
    top: u16;
    width: u16;
    height: u16;
    packed_fields: byte;
}

struct LocalColorTable {
    entries: []RGB {
        count: (image_descriptor.packed_fields & 0x07) + 1;
    };
}

struct ImageData {
    lzw_minimum_code_size: byte;
    sub_blocks: []SubBlock;
}

struct SubBlock {
    size: byte;
    data: [size]byte;
}

struct GIFTrailer {
    trailer: byte; // 0x3B
}

struct GIF {
    header: GIFHeader;
    logical_screen_descriptor: LogicalScreenDescriptor;
    global_color_table: ColorTable if logical_screen_descriptor.packed_fields & 0x80;
    image_descriptor: ImageDescriptor;
    local_color_table: LocalColorTable if image_descriptor.packed_fields & 0x80;
    image_data: ImageData;
    trailer: GIFTrailer;
}