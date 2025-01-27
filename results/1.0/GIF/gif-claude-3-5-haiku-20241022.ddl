file Gif {
    magic: [0x47, 0x49, 0x46, 0x38, 0x39, 0x61];  // GIF89a header
    
    logical_screen_descriptor: {
        width: u16_le,
        height: u16_le,
        packed_fields: u8,
        background_color_index: u8,
        pixel_aspect_ratio: u8
    }
    
    color_table_optional: match (packed_fields & 0x80) {
        1 => color_table(2 << (packed_fields & 0x07)),
        _ => void
    }
    
    blocks: repeat {
        block_type: u8,
        match block_type {
            0x21 => extension_block,
            0x2C => image_descriptor,
            0x3B => end_of_file,
            _ => error
        }
    }
    
    extension_block: {
        label: u8,
        block_size: u8,
        data: bytes(block_size)
    }
    
    image_descriptor: {
        image_separator: [0x2C],
        left_position: u16_le,
        top_position: u16_le,
        width: u16_le,
        height: u16_le,
        packed_fields: u8,
        local_color_table: match (packed_fields & 0x80) {
            1 => color_table(2 << (packed_fields & 0x07)),
            _ => void
        },
        lzw_minimum_code_size: u8,
        image_data_blocks: repeat_until_terminator(0)
    }
    
    image_data_blocks: {
        block_size: u8,
        block_data: bytes(block_size)
    }
    
    end_of_file: [0x3B]
    
    color_table: (size: int) => repeat(size) {
        red: u8, 
        green: u8, 
        blue: u8
    }
}