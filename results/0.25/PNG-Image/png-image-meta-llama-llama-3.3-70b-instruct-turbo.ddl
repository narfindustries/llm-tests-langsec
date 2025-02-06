format png_image {
  magic_number: byte[8] = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A],
  ihdr: ihdr_chunk,
  plte: optional plte_chunk,
  idat: array of idat_chunk,
  ancillary: array of ancillary_chunk,
  iend: iend_chunk
}

format ihdr_chunk {
  length: uint32 = 0x00000013,
  chunk_type: byte[4] = [0x49, 0x48, 0x44, 0x52],
  width: uint32,
  height: uint32,
  bit_depth: byte,
  color_type: color_type,
  compression_method: compression_method,
  filter_method: filter_method,
  interlace_method: interlace_method,
  crc: uint32
}

format plte_chunk {
  length: uint32,
  chunk_type: byte[4] = [0x50, 0x4C, 0x54, 0x45],
  palette_entries: array of plte_palette_entry,
  crc: uint32
}

format plte_palette_entry {
  red: byte,
  green: byte,
  blue: byte
}

format idat_chunk {
  length: uint32,
  chunk_type: byte[4] = [0x49, 0x44, 0x41, 0x54],
  compressed_image_data: array of byte,
  crc: uint32
}

format ancillary_chunk {
  length: uint32,
  chunk_type: byte[4],
  chunk_data: array of byte,
  crc: uint32
}

format iend_chunk {
  length: uint32 = 0,
  chunk_type: byte[4] = [0x49, 0x45, 0x4E, 0x44],
  crc: uint32
}

enum color_type {
  grayscale = 0,
  rgb = 2,
  plte = 3,
  grayscale_alpha = 4,
  rgba = 6
}

enum compression_method {
  deflate = 0
}

enum filter_method {
  adaptive = 0
}

enum interlace_method {
  none = 0,
  adam7 = 1
}

enum chunk_type {
  ihdr = [0x49, 0x48, 0x44, 0x52],
  plte = [0x50, 0x4C, 0x54, 0x45],
  idat = [0x49, 0x44, 0x41, 0x54],
  iend = [0x49, 0x45, 0x4E, 0x44],
  bkgd = [0x62, 0x4B, 0x47, 0x44],
  chrm = [0x63, 0x48, 0x52, 0x4D],
  gama = [0x67, 0x41, 0x4D, 0x41],
  hist = [0x68, 0x49, 0x53, 0x54],
  iccp = [0x69, 0x43, 0x43, 0x50],
  itxt = [0x69, 0x54, 0x58, 0x74],
  phys = [0x70, 0x48, 0x59, 0x73],
  sbit = [0x73, 0x42, 0x49, 0x54],
  splt = [0x73, 0x50, 0x4C, 0x54],
  srgb = [0x73, 0x52, 0x47, 0x42],
  text = [0x74, 0x45, 0x58, 0x74],
  time = [0x74, 0x49, 0x4D, 0x45],
  trns = [0x74, 0x52, 0x4E, 0x53],
  ztxt = [0x7A, 0x54, 0x58, 0x74]
}