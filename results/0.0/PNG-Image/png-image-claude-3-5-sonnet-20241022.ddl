def Main = {
  PNG_SIGNATURE
  IHDR_Chunk
  IDAT_Chunk*
  IEND_Chunk
}

def PNG_SIGNATURE = bytes ([0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A])

def Chunk = {
  length : uint32BE
  type   : FourCC
  data   : uint8[length]
  crc    : uint32BE
}

def FourCC = bytes (4)

def IHDR_Chunk = {
  chunk : Chunk where chunk.type == "IHDR"
  width  : uint32BE
  height : uint32BE
  depth  : uint8
  color  : uint8
  comp   : uint8
  filter : uint8
  inter  : uint8
}

def IDAT_Chunk = {
  chunk : Chunk where chunk.type == "IDAT"
}

def IEND_Chunk = {
  chunk : Chunk where chunk.type == "IEND"
}