type gif {
  header: {
    signature: byte[3] = "GIF",
    version: byte[3] = ["87a", "89a"]
  },
  logical_screen_descriptor: {
    width: uint16,
    height: uint16,
    packed_fields: {
      global_color_table_flag: bool,
      color_resolution: uint3,
      sort_flag: bool,
      size_of_global_color_table: uint3
    },
    background_color_index: uint8,
    pixel_aspect_ratio: uint8
  },
  global_color_table: array(2^(1+logical_screen_descriptor.packed_fields.size_of_global_color_table)) of {
    red: uint8,
    green: uint8,
    blue: uint8
  },
  image_descriptors: array(*) of {
    image_separator: byte = 0x2C,
    left_position: uint16,
    top_position: uint16,
    width: uint16,
    height: uint16,
    packed_fields: {
      local_color_table_flag: bool,
      interlace_flag: bool,
      sort_flag: bool,
      reserved: uint2 = 0,
      size_of_local_color_table: uint3
    },
    local_color_table: array(2^(1+packed_fields.size_of_local_color_table)) of {
      red: uint8,
      green: uint8,
      blue: uint8
    },
    image_data: byte(*)
  },
  extensions: array(*) of {
    extension_introducer: byte = 0x21,
    label: byte = [0xF9, 0xFE, 0xFF],
    block_size: byte,
    data: choice(label) of {
      0xF9: {
        disposal_method: uint3,
        user_input_flag: bool,
        transparent_color_flag: bool,
        delay_time: uint16,
        transparent_color_index: uint8
      },
      0xFE: {
        comment_data: byte(block_size)
      },
      0xFF: {
        application_identifier: byte(block_size),
        application_data: byte(*)
      }
    },
    block_terminator: byte = 0x00
  },
  trailer: byte = 0x3B
}