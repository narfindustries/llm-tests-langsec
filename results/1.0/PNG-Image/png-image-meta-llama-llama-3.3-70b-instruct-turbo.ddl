domain PNG-Image {
  import Daedalus.Builtins

  type PNG-Image {
    signature: uint32 = 0x89504E47
    ihdr: IHDR-Chunk
    idat: IDAT-Chunk
    iend: IEND-Chunk
  }

  type IHDR-Chunk {
    length: uint32 = 13
    type: uint32 = 0x49484452
    width: uint32
    height: uint32
    bit-depth: uint8
    color-type: uint8
    compression-method: uint8
    filter-method: uint8
    interlace-method: uint8
    crc: uint32
  }

  type IDAT-Chunk {
    length: uint32
    type: uint32 = 0x49444154
    data: bytes(length)
    crc: uint32
  }

  type IEND-Chunk {
    length: uint32 = 0
    type: uint32 = 0x49454E44
    crc: uint32 = 0xAE426082
  }
}