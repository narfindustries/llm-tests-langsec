def PNG {
  let magic = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
  magic: bytes[8]
  chunks: Chunk[]
}

def Chunk {
  length: u32
  type: bytes[4]
  data: select(type) {
    "IHDR" => IHDR
    "PLTE" => PLTE
    "IDAT" => IDAT
    "IEND" => IEND
    "tRNS" => TRNS
    "cHRM" => CHRM
    "gAMA" => GAMA
    "iCCP" => ICCP
    "sBIT" => SBIT
    "sRGB" => SRGB
    "tEXt" => TEXT
    "zTXt" => ZTXT
    "iTXt" => ITXT
    "bKGD" => BKGD
    "pHYs" => PHYS
    "tIME" => TIME
    _ => bytes[length]
  }
  crc: u32
}

def IHDR {
  width: u32
  height: u32
  bit_depth: u8
  color_type: u8
  compression_method: u8
  filter_method: u8
  interlace_method: u8
}

def PLTE {
  entries: RGB[]
}

def RGB {
  r: u8
  g: u8
  b: u8
}

def IDAT {
  data: bytes[]
}

def IEND {
}

def TRNS {
  data: bytes[]
}

def CHRM {
  white_point_x: u32
  white_point_y: u32
  red_x: u32
  red_y: u32
  green_x: u32
  green_y: u32
  blue_x: u32
  blue_y: u32
}

def GAMA {
  gamma: u32
}

def ICCP {
  name: string
  compression_method: u8
  compressed_profile: bytes[]
}

def SBIT {
  significant_bits: bytes[4]
}

def SRGB {
  rendering_intent: u8
}

def TEXT {
  keyword: string
  text: bytes[]
}

def ZTXT {
  keyword: string
  compression_method: u8
  compressed_text: bytes[]
}

def ITXT {
  keyword: string
  compression_flag: u8
  compression_method: u8
  language_tag: string
  translated_keyword: string
  text: bytes[]
}

def BKGD {
  background: bytes[6]
}

def PHYS {
  pixels_per_unit_x: u32
  pixels_per_unit_y: u32
  unit_specifier: u8
}

def TIME {
  year: u16
  month: u8
  day: u8
  hour: u8
  minute: u8
  second: u8
}