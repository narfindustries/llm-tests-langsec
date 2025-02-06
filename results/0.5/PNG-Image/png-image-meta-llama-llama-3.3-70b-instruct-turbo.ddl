format png {
  signature: byte[8] = b'\x89PNG\r\n\x1a\n',
  chunks: Chunk[]
}

format Chunk {
  length: uint32,
  type: byte[4],
  data: byte[length],
  crc: uint32
}

format IHDR {
  width: uint32,
  height: uint32,
  bit_depth: uint8,
  color_type: uint8,
  compression_method: uint8,
  filter_method: uint8,
  interlace_method: uint8
}

format PLTE {
  entries: PaletteEntry[]
}

format PaletteEntry {
  red: uint8,
  green: uint8,
  blue: uint8
}

format IDAT {
  data: byte[length]
}

format IEND {
}

format tRNS {
  values: TransparencyValue[]
}

format TransparencyValue {
  gray: uint16,
  red: uint16,
  green: uint16,
  blue: uint16
}

format cHRM {
  white_point_x: uint32,
  white_point_y: uint32,
  red_x: uint32,
  red_y: uint32,
  green_x: uint32,
  green_y: uint32,
  blue_x: uint32,
  blue_y: uint32
}

format gAMA {
  gamma: uint32
}

format iCCP {
  profile_name: string,
  compressed_profile: byte[length]
}

format sBIT {
  significant_bits: uint8
}

format sRGB {
  rendering_intent: uint8
}

format tEXt {
  keyword: string,
  text: string
}

format zTXt {
  keyword: string,
  compressed_text: byte[length]
}

format iTXt {
  keyword: string,
  compression_method: uint8,
  compressed_text: byte[length]
}

format bKGD {
  color: uint32
}

format hIST {
  entries: HistogramEntry[]
}

format HistogramEntry {
  frequency: uint16
}

format pHYs {
  pixels_per_unit: uint32,
  unit: uint8
}

format sPLT {
  palette_name: string,
  entries: PaletteEntry[]
}

enum ChunkType {
  IHDR = b'IHDR',
  PLTE = b'PLTE',
  IDAT = b'IDAT',
  IEND = b'IEND',
  tRNS = b'tRNS',
  cHRM = b'cHRM',
  gAMA = b'gAMA',
  iCCP = b'iCCP',
  sBIT = b'sBIT',
  sRGB = b'sRGB',
  tEXt = b'tEXt',
  zTXt = b'zTXt',
  iTXt = b'iTXt',
  bKGD = b'bKGD',
  hIST = b'hIST',
  pHYs = b'pHYs',
  sPLT = b'sPLT'
}

rule ChunkTypeRule {
  when type == b'IHDR' then IHDR,
  when type == b'PLTE' then PLTE,
  when type == b'IDAT' then IDAT,
  when type == b'IEND' then IEND,
  when type == b'tRNS' then tRNS,
  when type == b'cHRM' then cHRM,
  when type == b'gAMA' then gAMA,
  when type == b'iCCP' then iCCP,
  when type == b'sBIT' then sBIT,
  when type == b'sRGB' then sRGB,
  when type == b'tEXt' then tEXt,
  when type == b'zTXt' then zTXt,
  when type == b'iTXt' then iTXt,
  when type == b'bKGD' then bKGD,
  when type == b'hIST' then hIST,
  when type == b'pHYs' then pHYs,
  when type == b'sPLT' then sPLT
}