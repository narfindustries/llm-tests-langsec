def PNG_Image = {
  magic : uint8[8] where magic == [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]
  chunks : Chunk[]
}

def Chunk = {
  length : BE uint32
  type   : FourCC
  data   : uint8[length]
  crc    : BE uint32
}

def FourCC = uint8[4]

def IHDR = {
  width             : BE uint32
  height            : BE uint32
  bit_depth         : uint8
  color_type        : uint8
  compression       : uint8 where compression == 0
  filter_method     : uint8 where filter_method == 0
  interlace_method  : uint8 where interlace_method == 0 || interlace_method == 1
}

def IDAT = uint8[]

def IEND = null

def PLTE = {
  entries : RGB[]
}

def RGB = {
  r : uint8
  g : uint8
  b : uint8
}

def tRNS = {
  alpha_values : uint8[]
}

def gAMA = {
  gamma : BE uint32
}

def cHRM = {
  white_point_x : BE uint32
  white_point_y : BE uint32
  red_x         : BE uint32
  red_y         : BE uint32
  green_x       : BE uint32
  green_y       : BE uint32
  blue_x        : BE uint32
  blue_y        : BE uint32
}

def sRGB = {
  rendering_intent : uint8 where rendering_intent <= 3
}

def iCCP = {
  profile_name    : ZString
  compression     : uint8 where compression == 0
  profile         : uint8[]
}

def ZString = {
  chars : uint8[] until $$ == 0
  null  : uint8 where null == 0
}