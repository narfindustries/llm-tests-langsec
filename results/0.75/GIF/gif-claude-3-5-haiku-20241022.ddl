type GIF = {
    header: {
        signature: [3]u8 where { == b"GIF" },
        version: [3]u8 where { == b"87a" || == b"89a" }
    },
    logical_screen_descriptor: {
        width: u16le,
        height: u16le,
        packed_fields: u8 {
            global_color_table_flag: u1,
            color_resolution: u3,
            sort_flag: u1,
            global_color_table_size: u3
        },
        background_color_index: u8,
        pixel_aspect_ratio: u8
    },
    global_color_table: if logical_screen_descriptor.packed_fields.global_color_table_flag == 1 then 
        [2^(logical_screen_descriptor.packed_fields.global_color_table_size + 1)] {
            red: u8,
            green: u8,
            blue: u8
        }
    else [],
    blocks: *{
        | ImageDescriptor: {
            separator: u8 where { == 0x2C },
            left_position: u16le,
            top_position: u16le,
            width: u16le,
            height: u16le,
            packed_fields: u8 {
                local_color_table_flag: u1,
                interlace_flag: u1,
                sort_flag: u1,
                local_color_table_size: u3
            },
            local_color_table: if packed_fields.local_color_table_flag == 1 then
                [2^(packed_fields.local_color_table_size + 1)] {
                    red: u8,
                    green: u8,
                    blue: u8
                }
            else [],
            image_data: {
                lzw_minimum_code_size: u8,
                data_blocks: *{
                    block_size: u8,
                    data: [block_size]u8
                } where { block_size != 0 },
                terminator: u8 where { == 0 }
            }
        }
        | GraphicControlExtension: {
            extension_introducer: u8 where { == 0x21 },
            graphic_control_label: u8 where { == 0xF9 },
            block_size: u8 where { == 4 },
            packed_fields: u8 {
                reserved: u3,
                disposal_method: u3,
                user_input_flag: u1,
                transparent_color_flag: u1
            },
            delay_time: u16le,
            transparent_color_index: u8,
            block_terminator: u8 where { == 0 }
        }
        | CommentExtension: {
            extension_introducer: u8 where { == 0x21 },
            comment_label: u8 where { == 0xFE },
            comment_blocks: *{
                block_size: u8,
                data: [block_size]u8
            } where { block_size != 0 },
            block_terminator: u8 where { == 0 }
        }
        | ApplicationExtension: {
            extension_introducer: u8 where { == 0x21 },
            application_label: u8 where { == 0xFF },
            block_size: u8 where { == 11 },
            application_identifier: [8]u8,
            authentication_code: [3]u8,
            data_blocks: *{
                block_size: u8,
                data: [block_size]u8
            } where { block_size != 0 },
            block_terminator: u8 where { == 0 }
        }
    },
    trailer: u8 where { == 0x3B }
}