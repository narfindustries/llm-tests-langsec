format gif {
  signature: bytes:3 = "GIF",
  version: bytes:3 = ["87a", "89a"],
  logical_screen_width: uint16:le,
  logical_screen_height: uint16:le,
  packed_fields: bits {
    global_color_table_flag: 1,
    color_resolution: 3,
    sort_flag: 1,
    color_table_size: 3
  },
  background_color_index: byte,
  pixel_aspect_ratio: byte,
  global_color_table: optional bytes:3[2:256],
  images: array image,
  trailer: byte = 0x3B
}

format image {
  image_separator: byte = 0x2C,
  image_left_position: uint16:le,
  image_top_position: uint16:le,
  image_width: uint16:le,
  image_height: uint16:le,
  packed_fields: bits {
    local_color_table_flag: 1,
    interlace_flag: 1,
    sort_flag: 1,
    color_table_size: 3
  },
  local_color_table: optional bytes:3[2:256],
  image_data: lzw_compressed
}

format lzw_compressed {
  compressed_data: bytes
}

type uint16:le = uint16(little_endian)
type byte = uint8
type uint8 = uint(8)
type uint16 = uint(16)
type uint = int(32, unsigned)

type bits {
  bit: bitfield_member*
}

type bitfield_member {
  name: string,
  size: int
}

type array {
  type: type,
  length: int
}

type optional {
  type: type
}

type little_endian {
  endianness: little
}