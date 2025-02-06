type GIF = {
    signature: [3]u8 where (
        signature[0] == 'G' and 
        signature[1] == 'I' and 
        signature[2] == 'F'
    ),
    version: [3]u8 where (
        version == "87a" or 
        version == "89a"
    ),
    logical_screen_descriptor: {
        width: u16 le,
        height: u16 le,
        packed_fields: u8,
        background_color_index: u8,
        pixel_aspect_ratio: u8
    },
    global_color_table: optional([3]u8 * (pow(2, ((logical_screen_descriptor.packed_fields >> 5) & 0x07) + 1))),
    blocks: [Block] while (last(blocks) != Trailer)
};

type Block = 
    | ImageDescriptor
    | GraphicControlExtension
    | ApplicationExtension
    | CommentExtension
    | Trailer;

type ImageDescriptor = {
    separator: u8 where (separator == 0x2C),
    left_position: u16 le,
    top_position: u16 le,
    width: u16 le,
    height: u16 le,
    packed_fields: u8,
    local_color_table: optional([3]u8 * (pow(2, (packed_fields & 0x07) + 1))),
    lzw_min_code_size: u8,
    image_data: [u8] while (last(image_data) != 0)
};

type GraphicControlExtension = {
    extension_introducer: u8 where (extension_introducer == 0x21),
    graphic_control_label: u8 where (graphic_control_label == 0xF9),
    block_size: u8 where (block_size == 0x04),
    packed_fields: u8,
    delay_time: u16 le,
    transparent_color_index: u8,
    block_terminator: u8 where (block_terminator == 0)
};

type ApplicationExtension = {
    extension_introducer: u8 where (extension_introducer == 0x21),
    application_label: u8 where (application_label == 0xFF),
    block_size: u8 where (block_size == 0x0B),
    application_identifier: [8]u8,
    authentication_code: [3]u8,
    data_blocks: [u8] while (last(data_blocks) != 0)
};

type CommentExtension = {
    extension_introducer: u8 where (extension_introducer == 0x21),
    comment_label: u8 where (comment_label == 0xFE),
    comment_data: [u8] while (last(comment_data) != 0)
};

type Trailer = {
    trailer: u8 where (trailer == 0x3B)
};