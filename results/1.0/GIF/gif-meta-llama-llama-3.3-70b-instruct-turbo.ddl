format gif {
  signature: bytes(3) = "GIF";
  version: bytes(3) = "87a" | "89a";
  logical_screen_width: uint16le;
  logical_screen_height: uint16le;
  packed_fields: struct {
    uint8 value;
    bits {
      global_color_table_flag: 1;
      color_resolution: 3;
      sort_flag: 1;
      size_of_global_color_table: 3;
    }
  };
  background_color_index: uint8;
  pixel_aspect_ratio: uint8;
  global_color_table: array uint8[3 * (2 ^ packed_fields.size_of_global_color_table)];
  images: array image_descriptor;
  trailer: uint8 = 0x3B;
}

format image_descriptor {
  image_separator: uint8 = 0x2C;
  image_left_position: uint16le;
  image_top_position: uint16le;
  image_width: uint16le;
  image_height: uint16le;
  packed_fields: struct {
    uint8 value;
    bits {
      local_color_table_flag: 1;
      interlace_flag: 1;
      sort_flag: 1;
      reserved: 4;
      size_of_local_color_table: 3;
    }
  };
  local_color_table: array uint8[3 * (2 ^ packed_fields.size_of_local_color_table)];
  image_data: array lzw_compressed_data;
  graphic_control_extension: optional graphic_control_extension;
  comment_extension: optional comment_extension;
  application_extension: optional application_extension;
}

format graphic_control_extension {
  extension_introducer: uint8 = 0x21;
  label: uint8 = 0xF9;
  block_size: uint8 = 4;
  packed_fields: struct {
    uint8 value;
    bits {
      reserved: 3;
      disposal_method: 3;
      user_input_flag: 1;
      transparency_flag: 1;
    }
  };
  delay_time: uint16le;
  transparent_color_index: uint8;
}

format comment_extension {
  extension_introducer: uint8 = 0x21;
  label: uint8 = 0xFE;
  comment: string;
}

format application_extension {
  extension_introducer: uint8 = 0x21;
  label: uint8 = 0xFF;
  application_identifier: string;
  authentication_code: bytes(3);
  application_data: string;
}

format lzw_compressed_data {
  compressed_data: bytes;
}