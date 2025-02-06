def png:
  signature: bytes = b"\x89PNG\r\n\x1a\n"
  chunks: [chunk, ...]

  def chunk_type:
    enum = ["IHDR", "PLTE", "IDAT", "IEND", "cHRM", "gAMA", "iCCP", "sBIT", "sRGB", "tEXt", "tIME", "bKGD", "hIST", "pHYs", "sPLT", "tRNS"]

  def chunk:
    length: int
    type: chunk_type
    data: bytes = length
    crc: int

  def ihdr:
    width: int
    height: int
    bit_depth: int
    color_type: int
    compression_method: int
    filter_method: int
    interlace_method: int

  def plte:
    palette: [rgb, ...]

  def rgb:
    r: int
    g: int
    b: int

  def idat:
    data: bytes

  def iend:
    pass

  def chrm:
    white_point_x: int
    white_point_y: int
    red_x: int
    red_y: int
    green_x: int
    green_y: int
    blue_x: int
    blue_y: int

  def gama:
    gamma: int

  def iccp:
    profile_name: bytes
    compression_method: int
    compressed_profile: bytes

  def sbit:
    significant_bits: [int, int, int]

  def srgb:
    rendering_intent: int

  def text:
    keyword: bytes
    text: bytes

  def time:
    year: int
    month: int
    day: int
    hour: int
    minute: int
    second: int

  def bkgd:
    color: rgb

  def hist:
    histogram: [int, ...]

  def phys:
    pixels_per_unit_x: int
    pixels_per_unit_y: int
    unit_specifier: int

  def splte:
    palette: [rgb, ...]

  def trns:
    transparency: bytes

  assert chunks[0].type == "IHDR"
  assert chunks[-1].type == "IEND"