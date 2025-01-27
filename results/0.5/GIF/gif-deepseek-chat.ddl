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
    colors: vec[3] {
        red: u8;
        green: u8;
        blue: u8;
    };
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
    sub_blocks: vec {
        block_size: u8;
        data: bytes[block_size];
    } until block_size == 0;
}

struct GIF {
    header: GIFHeader;
    logical_screen_descriptor: LogicalScreenDescriptor;
    global_color_table: if logical_screen_descriptor.packed_fields & 0x80 != 0 {
        ColorTable[2^(logical_screen_descriptor.packed_fields & 0x07 + 1)];
    };
    blocks: vec {
        block_type: u8;
        block: switch block_type {
            0x2C => ImageBlock;
            0x21 => ExtensionBlock;
            0x3B => EndBlock;
        };
    } until block_type == 0x3B;
}

struct ImageBlock {
    image_descriptor: ImageDescriptor;
    local_color_table: if image_descriptor.packed_fields & 0x80 != 0 {
        ColorTable[2^(image_descriptor.packed_fields & 0x07 + 1)];
    };
    image_data: ImageData;
}

struct ExtensionBlock {
    extension_label: u8;
    block_size: u8;
    data: bytes[block_size];
    sub_blocks: vec {
        block_size: u8;
        data: bytes[block_size];
    } until block_size == 0;
}

struct EndBlock {
    terminator: u8;
}