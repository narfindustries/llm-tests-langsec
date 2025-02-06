format gif {
  header: bytes(3) = "GIF",
  version: bytes(3) = choice("87a", "89a"),
  logical_screen_descriptor: {
    logical_screen_width: uint16,
    logical_screen_height: uint16,
    packed_fields: bits(8) = {
      global_color_table_flag: bit(1),
      color_resolution: bits(3),
      sort_flag: bit(1),
      size_of_global_color_table: bits(3)
    },
    background_color_index: uint8,
    pixel_aspect_ratio: uint8
  },
  global_color_table: optional(array(uint8, 3 * (2 ^ (1 + logical_screen_descriptor.packed_fields.size_of_global_color_table)))),
  images: array({
    image_separator: byte = 0x2C,
    image_left: uint16,
    image_top: uint16,
    image_width: uint16,
    image_height: uint16,
    packed_fields: bits(8) = {
      local_color_table_flag: bit(1),
      interlace_flag: bit(1),
      sort_flag: bit(1),
      reserved: bits(2),
      size_of_local_color_table: bits(3)
    },
    local_color_table: optional(array(uint8, 3 * (2 ^ (1 + packed_fields.size_of_local_color_table)))),
    image_data: bytes,
    graphic_control_extension: optional({
      extension_introducer: byte = 0x21,
      extension_label: byte = 0xF9,
      block_size: uint8,
      packed_fields: bits(8) = {
        reserved: bits(3),
        disposal_method: bits(3),
        user_input_flag: bit(1),
        transparent_color_flag: bit(1)
      },
      delay_time: uint16,
      transparent_color_index: optional(uint8)
    }),
    comment_extension: optional({
      extension_introducer: byte = 0x21,
      extension_label: byte = 0xFE,
      block_size: uint8,
      comment_data: bytes
    }),
    application_extension: optional({
      extension_introducer: byte = 0x21,
      extension_label: byte = 0xFF,
      block_size: uint8,
      application_identifier: bytes,
      authentication_code: bytes,
      application_data: bytes
    })
  }, *),
  trailer: byte = 0x3B
}