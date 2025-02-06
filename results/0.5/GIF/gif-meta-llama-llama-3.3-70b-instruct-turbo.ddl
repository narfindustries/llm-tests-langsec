define format gif {
  signature: 3*byte = "GIF",
  version: 3*byte = "87a" | "89a",

  logical_screen_descriptor: {
    logical_screen_width: uint16le,
    logical_screen_height: uint16le,
    packed_fields: byte,
    global_color_table_flag: bool = ((packed_fields & 0x01) != 0),
    color_resolution: uint3 = ((packed_fields & 0x1E) >> 1),
    sort_flag: bool = ((packed_fields & 0x20) != 0),
    size_of_global_color_table: uint3 = ((packed_fields & 0xE0) >> 5),

    background_color_index: byte,
    pixel_aspect_ratio: byte,
  },

  global_color_table: array {
    size: 3*(2 ** (logical_screen_descriptor.size_of_global_color_table + 1)),
    condition: logical_screen_descriptor.global_color_table_flag,
    red: byte,
    green: byte,
    blue: byte,
  },

  image_descriptors: array {
    image_separator: byte = 0x2C,
    image_left_position: uint16le,
    image_top_position: uint16le,
    image_width: uint16le,
    image_height: uint16le,
    packed_fields: byte,
    local_color_table_flag: bool = ((packed_fields & 0x01) != 0),
    interlace_flag: bool = ((packed_fields & 0x02) != 0),
    sort_flag: bool = ((packed_fields & 0x04) != 0),
    size_of_local_color_table: uint3 = ((packed_fields & 0x80) >> 7),

    local_color_table: array {
      size: 3*(2 ** (size_of_local_color_table + 1)),
      condition: local_color_table_flag,
      red: byte,
      green: byte,
      blue: byte,
    },

    image_data: byte[variable],
  }[variable],

  trailer: byte = 0x3B,
}