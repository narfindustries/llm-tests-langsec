def GIF = {
    signature: bytes(3) where bytes == b"GIF",
    version: bytes(3) where bytes == b"87a" or bytes == b"89a",
    screen_descriptor: {
        width: u16,
        height: u16,
        packed_fields: {
            global_color_table_flag: bit,
            color_resolution: bits(3),
            sort_flag: bit,
            global_color_table_size: bits(3)
        },
        background_color_index: u8,
        pixel_aspect_ratio: u8
    },
    global_color_table: optional({
        entries: list({
            red: u8,
            green: u8,
            blue: u8
        })
    }),
    blocks: list(
        variant {
            image_block: {
                separator: u8 where value == 0x2C,
                left_position: u16,
                top_position: u16,
                width: u16,
                height: u16,
                packed_fields: {
                    local_color_table_flag: bit,
                    interlace_flag: bit,
                    sort_flag: bit,
                    local_color_table_size: bits(3)
                },
                local_color_table: optional({
                    entries: list({
                        red: u8,
                        green: u8,
                        blue: u8
                    })
                }),
                image_data: {
                    lzw_min_code_size: u8,
                    data_blocks: list({
                        length: u8,
                        data: bytes(length)
                    })
                }
            },
            extension_block: variant {
                graphic_control: {
                    introducer: u8 where value == 0x21,
                    label: u8 where value == 0xF9,
                    block_size: u8 where value == 4,
                    packed_fields: {
                        disposal_method: bits(3),
                        user_input_flag: bit,
                        transparent_color_flag: bit
                    },
                    delay_time: u16,
                    transparent_color_index: u8
                },
                comment: {
                    introducer: u8 where value == 0x21,
                    label: u8 where value == 0xFE,
                    comment_blocks: list({
                        length: u8,
                        data: bytes(length)
                    })
                },
                plain_text: {
                    introducer: u8 where value == 0x21,
                    label: u8 where value == 0x01,
                    block_size: u8,
                    text_grid_left_position: u16,
                    text_grid_top_position: u16,
                    text_grid_width: u16,
                    text_grid_height: u16,
                    cell_width: u8,
                    cell_height: u8,
                    foreground_color_index: u8,
                    background_color_index: u8,
                    plain_text_data: list({
                        length: u8,
                        data: bytes(length)
                    })
                },
                application: {
                    introducer: u8 where value == 0x21,
                    label: u8 where value == 0xFF,
                    block_size: u8 where value == 11,
                    application_identifier: bytes(8),
                    authentication_code: bytes(3),
                    application_data_blocks: list({
                        length: u8,
                        data: bytes(length)
                    })
                }
            }
        }
    ),
    trailer: u8 where value == 0x3B
}