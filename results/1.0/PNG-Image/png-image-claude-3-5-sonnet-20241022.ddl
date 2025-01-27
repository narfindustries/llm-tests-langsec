def PNG_SIGNATURE = [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

def PNG_IHDR = "IHDR"
def PNG_IDAT = "IDAT" 
def PNG_IEND = "IEND"

def CRC = UInt32BE

struct Chunk {
  length: UInt32BE
  type: FString 4
  data: Bytes length
  crc: CRC
}

struct IHDR {
  width: UInt32BE
  height: UInt32BE
  bitDepth: UInt8
  colorType: UInt8
  compression: UInt8
  filter: UInt8
  interlace: UInt8
}

struct PNGImage {
  signature: Bytes 8
  @requires signature == PNG_SIGNATURE
  
  ihdrChunk: Chunk
  @requires ihdrChunk.type == PNG_IHDR
  ihdrData: IHDR = IHDR @ ihdrChunk.data
  
  idatChunks: Chunk[]
  @requires forall chunk in idatChunks { chunk.type == PNG_IDAT }
  
  iendChunk: Chunk  
  @requires iendChunk.type == PNG_IEND
  @requires iendChunk.length == 0
}

def Main = PNGImage