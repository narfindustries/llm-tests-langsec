PNGImage {
  signature: U8[8] = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A],
  chunks: Chunk[],
}

Chunk {
  length: U32,
  type: U8[4],
  data: U8[length],
  crc: U32,
}

IHDR {
  width: U32,
  height: U32,
  bit_depth: U8,
  color_type: U8,
  compression_method: U8 = 0,
  filter_method: U8 = 0,
  interlace_method: U8,
}

PLTE {
  entries: RGB[],
}

RGB {
  red: U8,
  green: U8,
  blue: U8,
}

IDAT {
  data: U8[length],
}

IEND {
}

tRNS {
  data: U8[length],
}

gAMA {
  gamma: U32,
}

cHRM {
  white_point_x: U32,
  white_point_y: U32,
  red_x: U32,
  red_y: U32,
  green_x: U32,
  green_y: U32,
  blue_x: U32,
  blue_y: U32,
}

sRGB {
  rendering_intent: U8,
}

iCCP {
  profile_name: U8[],
  compression_method: U8 = 0,
  profile_data: U8[length],
}

tEXt {
  keyword: U8[],
  text: U8[],
}

zTXt {
  keyword: U8[],
  compression_method: U8 = 0,
  text: U8[length],
}

iTXt {
  keyword: U8[],
  compression_flag: U8,
  compression_method: U8 = 0,
  language_tag: U8[],
  translated_keyword: U8[],
  text: U8[length],
}

bKGD {
  data: U8[length],
}

hIST {
  frequencies: U16[],
}

pHYs {
  pixels_per_unit_x: U32,
  pixels_per_unit_y: U32,
  unit_specifier: U8,
}

sBIT {
  data: U8[length],
}

sPLT {
  palette_name: U8[],
  sample_depth: U8,
  entries: PaletteEntry[],
}

PaletteEntry {
  red: U8,
  green: U8,
  blue: U8,
  alpha: U8,
  frequency: U16,
}

tIME {
  year: U16,
  month: U8,
  day: U8,
  hour: U8,
  minute: U8,
  second: U8,
}