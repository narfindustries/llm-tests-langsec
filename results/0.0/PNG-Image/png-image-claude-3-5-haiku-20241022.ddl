type PNG-Image {
    signature: [137, 80, 78, 71, 13, 10, 26, 10],
    IHDR-chunk: IHDR-Chunk,
    other-chunks: [Chunk]*,
    IEND-chunk: IEND-Chunk
}

type IHDR-Chunk {
    length: u32 = 13,
    chunk-type: [73, 72, 68, 82],
    width: u32,
    height: u32,
    bit-depth: u8,
    color-type: u8,
    compression-method: u8,
    filter-method: u8,
    interlace-method: u8,
    crc: u32
}

type Chunk {
    length: u32,
    chunk-type: [u8]*,
    chunk-data: [u8]*,
    crc: u32
}

type IEND-Chunk {
    length: u32 = 0,
    chunk-type: [73, 69, 78, 68],
    crc: u32
}

parse PNG-Image {
    signature = read 8;
    IHDR-chunk = read IHDR-Chunk;
    other-chunks = read-while { !is-iend-chunk(current-chunk) } Chunk;
    IEND-chunk = read IEND-Chunk;
}

fn is-iend-chunk(chunk: Chunk) -> bool {
    chunk.chunk-type == [73, 69, 78, 68]
}