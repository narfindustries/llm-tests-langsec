// Daedalus DDL specification for a PNG image
format PNGImage {
    // PNG has a fixed signature
    signature '89 50 4E 47 0D 0A 1A 0A'

    // Chunk structure
    struct Chunk {
        length: u4
        type: string(4)
        data: bytes(length)
        crc: u4
    }

    // PNG image starts with a set of chunks
    chunks: list[Chunk] until { $.chunks[$.chunks.length - 1].type == "IEND" }
}