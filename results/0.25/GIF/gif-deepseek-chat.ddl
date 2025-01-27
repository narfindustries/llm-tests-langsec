struct GIFHeader {
    signature: [3]byte; // "GIF"
    version: [3]byte;   // "87a" or "89a"
}

struct LogicalScreenDescriptor {
    canvas_width: u16;
    canvas_height: u16;
    packed_fields: byte;
    background_color_index: byte;
    pixel_aspect_ratio: byte;
}

struct ColorTable {
    colors: []byte; // Size depends on packed_fields in LogicalScreenDescriptor
}

struct ImageDescriptor {
    separator: byte; // 0x2C
    left_position: u16;
    top_position: u16;
    width: u16;
    height: u16;
    packed_fields: byte;
}

struct LocalColorTable {
    colors: []byte; // Size depends on packed_fields in ImageDescriptor
}

struct ImageData {
    lzw_minimum_code_size: byte;
    compressed_data: []byte; // Size depends on the image data
}

struct GIFImage {
    descriptor: ImageDescriptor;
    local_color_table: LocalColorTable?; // Optional, depends on packed_fields
    image_data: ImageData;
}

struct GIFTrailer {
    trailer: byte; // 0x3B
}

struct GIF {
    header: GIFHeader;
    logical_screen_descriptor: LogicalScreenDescriptor;
    global_color_table: ColorTable?; // Optional, depends on packed_fields
    images: []GIFImage;
    trailer: GIFTrailer;
}