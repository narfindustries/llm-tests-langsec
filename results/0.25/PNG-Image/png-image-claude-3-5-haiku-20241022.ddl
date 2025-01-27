type PNG-Image {
    signature: [137, 80, 78, 71, 13, 10, 26, 10],
    IHDR-chunk: PNG-Chunk,
    other-chunks: [PNG-Chunk],
    IEND-chunk: PNG-Chunk
}

type PNG-Chunk {
    length: u32,
    chunk-type: [u8; 4],
    chunk-data: [u8; length],
    crc: u32
}

type Color-Type enum {
    Grayscale = 0,
    RGB = 2,
    Palette = 3,
    Grayscale-Alpha = 4,
    RGBA = 6
}

type IHDR-Data {
    width: u32,
    height: u32,
    bit-depth: u8,
    color-type: Color-Type,
    compression-method: u8,
    filter-method: u8,
    interlace-method: u8
}

parse PNG-Image {
    signature = read 8;
    ihdr-chunk = parse PNG-Chunk where chunk-type == "IHDR";
    other-chunks = parse-many PNG-Chunk 
        while chunk-type != "IEND";
    iend-chunk = parse PNG-Chunk where chunk-type == "IEND";
    
    return PNG-Image {
        signature: signature,
        IHDR-chunk: ihdr-chunk,
        other-chunks: other-chunks,
        IEND-chunk: iend-chunk
    }
}

parse PNG-Chunk {
    length = read-u32-be;
    chunk-type = read 4;
    chunk-data = read length;
    crc = read-u32-be;
    
    return PNG-Chunk {
        length: length,
        chunk-type: chunk-type,
        chunk-data: chunk-data,
        crc: crc
    }
}