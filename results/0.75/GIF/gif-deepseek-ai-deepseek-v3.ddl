GIF = struct {
    header: struct {
        signature: bytes(3) = "GIF";
        version: bytes(3) = "89a";
    };
    logical_screen_descriptor: struct {
        width: uint16;
        height: uint16;
        packed_fields: struct {
            global_color_table_flag: bit;
            color_resolution: uint3;
            sort_flag: bit;
            size_of_global_color_table: uint3;
        };
        background_color_index: uint8;
        pixel_aspect_ratio: uint8;
    };
    global_color_table: optional array[pow(2, logical_screen_descriptor.packed_fields.size_of_global_color_table + 1)] of struct {
        red: uint8;
        green: uint8;
        blue: uint8;
    };
    blocks: array[] of union {
        image_block: struct {
            image_separator: uint8 = 0x2C;
            image_left_position: uint16;
            image_top_position: uint16;
            image_width: uint16;
            image_height: uint16;
            packed_fields: struct {
                local_color_table_flag: bit;
                interlace_flag: bit;
                sort_flag: bit;
                reserved: uint2 = 0;
                size_of_local_color_table: uint3;
            };
            local_color_table: optional array[pow(2, packed_fields.size_of_local_color_table + 1)] of struct {
                red: uint8;
                green: uint8;
                blue: uint8;
            };
            image_data: struct {
                lzw_minimum_code_size: uint8;
                sub_blocks: array[] of struct {
                    block_size: uint8;
                    data: bytes(block_size);
                } until (block_size == 0);
            };
        };
        extension_block: struct {
            extension_introducer: uint8 = 0x21;
            label: union {
                graphic_control_extension: struct {
                    graphic_control_label: uint8 = 0xF9;
                    block_size: uint8 = 4;
                    packed_fields: struct {
                        reserved: uint3 = 0;
                        disposal_method: uint3;
                        user_input_flag: bit;
                        transparency_flag: bit;
                    };
                    delay_time: uint16;
                    transparency_index: uint8;
                    block_terminator: uint8 = 0x00;
                };
                plain_text_extension: struct {
                    plain_text_label: uint8 = 0x01;
                    block_size: uint8 = 12;
                    text_grid_left_position: uint16;
                    text_grid_top_position: uint16;
                    text_grid_width: uint16;
                    text_grid_height: uint16;
                    character_cell_width: uint8;
                    character_cell_height: uint8;
                    text_foreground_color_index: uint8;
                    text_background_color_index: uint8;
                    text_data: struct {
                        sub_blocks: array[] of struct {
                            block_size: uint8;
                            data: bytes(block_size);
                        } until (block_size == 0);
                    };
                    block_terminator: uint8 = 0x00;
                };
                comment_extension: struct {
                    comment_label: uint8 = 0xFE;
                    comment_data: struct {
                        sub_blocks: array[] of struct {
                            block_size: uint8;
                            data: bytes(block_size);
                        } until (block_size == 0);
                    };
                    block_terminator: uint8 = 0x00;
                };
                application_extension: struct {
                    application_label: uint8 = 0xFF;
                    block_size: uint8 = 11;
                    application_identifier: bytes(8);
                    application_authentication_code: bytes(3);
                    application_data: struct {
                        sub_blocks: array[] of struct {
                            block_size: uint8;
                            data: bytes(block_size);
                        } until (block_size == 0);
                    };
                    block_terminator: uint8 = 0x00;
                };
            };
        };
    } until (eof);
    trailer: uint8 = 0x3B;
};