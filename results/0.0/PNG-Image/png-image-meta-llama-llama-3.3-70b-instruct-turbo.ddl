format png_image {
  magic_number: byte[8] = {89, 50, 4E, 47, 0D, 0A, 1A, 0A},
  chunks: chunk[]
}

format chunk {
  length: uint32,
  type: byte[4],
  data: byte[length],
  crc: uint32
}

format ihdr_chunk {
  width: uint32,
  height: uint32,
  bit_depth: byte,
  color_type: byte,
  compression_method: byte,
  filter_method: byte,
  interlace_method: byte
}

format plte_chunk {
  palette_entries: rgb[length / 3]
}

format rgb {
  red: byte,
  green: byte,
  blue: byte
}

format idat_chunk {
  compressed_image_data: byte[length]
}

format iend_chunk {
}

format cHRM_chunk {
  white_point_x: uint32,
  white_point_y: uint32,
  red_x: uint32,
  red_y: uint32,
  green_x: uint32,
  green_y: uint32,
  blue_x: uint32,
  blue_y: uint32
}

format gAMA_chunk {
  gamma: uint32
}

format iCCP_chunk {
  profile_name: byte[length],
  compression_method: byte,
  compressed_profile: byte[length]
}

format sBIT_chunk {
  significant_bits: byte[3]
}

format sRGB_chunk {
  rendering_intent: byte
}

format tEXt_chunk {
  keyword: byte[length],
  text: byte[length]
}

format tIME_chunk {
  year: uint16,
  month: uint8,
  day: uint8,
  hour: uint8,
  minute: uint8,
  second: uint8
}

format tRNS_chunk {
  transparency: byte[length]
}

format zTXt_chunk {
  keyword: byte[length],
  compression_method: byte,
  compressed_text: byte[length]
}

constraint png_image {
  magic_number == {89, 50, 4E, 47, 0D, 0A, 1A, 0A},
  chunks[0].type == {73, 72, 68, 82},
  chunks[-1].type == {73, 69, 78, 68}
}

constraint chunk {
  crc == crc32(type, data)
}

constraint ihdr_chunk {
  bit_depth in {1, 2, 4, 8, 16},
  color_type in {0, 2, 3, 4, 6},
  compression_method == 0,
  filter_method == 0,
  interlace_method in {0, 1}
}

constraint plte_chunk {
  length % 3 == 0
}

constraint idat_chunk {
  compression_method == 0
}

constraint cHRM_chunk {
  white_point_x > 0,
  white_point_y > 0,
  red_x > 0,
  red_y > 0,
  green_x > 0,
  green_y > 0,
  blue_x > 0,
  blue_y > 0
}

constraint gAMA_chunk {
  gamma > 0
}

constraint iCCP_chunk {
  compression_method in {0, 1}
}

constraint sBIT_chunk {
  significant_bits[0] in {1, 2, 4, 8, 16},
  significant_bits[1] in {1, 2, 4, 8, 16},
  significant_bits[2] in {1, 2, 4, 8, 16}
}

constraint sRGB_chunk {
  rendering_intent in {0, 1, 2, 3}
}

constraint tEXt_chunk {
  keyword != "",
  text != ""
}

constraint tIME_chunk {
  year > 0,
  month in {1, 12},
  day in {1, 31},
  hour in {0, 23},
  minute in {0, 59},
  second in {0, 59}
}

constraint tRNS_chunk {
  length > 0
}

constraint zTXt_chunk {
  keyword != "",
  compression_method in {0, 1}
}