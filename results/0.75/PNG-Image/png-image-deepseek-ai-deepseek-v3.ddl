root {
  PNG {
    signature: U8[8] where $ == "\x89PNG\r\n\x1a\n",
    chunks: Chunk[] until $ == "IEND"
  }
}

Chunk {
  length: U32,
  type: U8[4],
  data: U8[length],
  crc: U32
}

IHDR {
  width: U32,
  height: U32,
  bit_depth: U8 where $ in [1, 2, 4, 8, 16],
  color_type: U8 where $ in [0, 2, 3, 4, 6],
  compression_method: U8 where $ == 0,
  filter_method: U8 where $ == 0,
  interlace_method: U8 where $ in [0, 1]
}

PLTE {
  entries: U8[3][]
}

IDAT {
  compressed_data: U8[]
}

IEND {}

tEXt {
  keyword: U8[] until $ == 0,
  text: U8[] until $ == 0
}

zTXt {
  keyword: U8[] until $ == 0,
  compression_method: U8 where $ == 0,
  compressed_text: U8[]
}

iTXt {
  keyword: U8[] until $ == 0,
  compression_flag: U8 where $ in [0, 1],
  compression_method: U8 where $ == 0,
  language_tag: U8[] until $ == 0,
  translated_keyword: U8[] until $ == 0,
  text: U8[]
}

bKGD {
  background_color: case root.PNG.chunks[0].data.color_type {
    0 => U16,
    2 => U16[3],
    3 => U8,
    4 => U16,
    6 => U16[3]
  }
}

cHRM {
  white_point_x: U32,
  white_point_y: U32,
  red_x: U32,
  red_y: U32,
  green_x: U32,
  green_y: U32,
  blue_x: U32,
  blue_y: U32
}

gAMA {
  gamma: U32
}

hIST {
  entries: U16[root.PNG.chunks.find($.type == "PLTE").length]
}

iCCP {
  profile_name: U8[] until $ == 0,
  compression_method: U8 where $ == 0,
  compressed_profile: U8[]
}

pHYs {
  pixels_per_unit_x: U32,
  pixels_per_unit_y: U32,
  unit_specifier: U8 where $ in [0, 1]
}

sBIT {
  significant_bits: case root.PNG.chunks[0].data.color_type {
    0 => U8,
    2 => U8[3],
    3 => U8[3],
    4 => U8[2],
    6 => U8[4]
  }
}

sPLT {
  palette_name: U8[] until $ == 0,
  sample_depth: U8 where $ in [8, 16],
  entries: U8[]
}

sRGB {
  rendering_intent: U8 where $ in [0, 1, 2, 3]
}

tIME {
  year: U16,
  month: U8 where $ >= 1 and $ <= 12,
  day: U8 where $ >= 1 and $ <= 31,
  hour: U8 where $ <= 23,
  minute: U8 where $ <= 59,
  second: U8 where $ <= 59
}

tRNS {
  transparency_data: case root.PNG.chunks[0].data.color_type {
    0 => U16,
    2 => U16[3],
    3 => U8,
    4 => NULL,
    6 => NULL
  }
}