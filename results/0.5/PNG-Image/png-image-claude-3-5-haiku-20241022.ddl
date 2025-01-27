type PNG-Image:
    signature: Bytes<8>
    ihdr_chunk: IHDR-Chunk
    data_chunks: [Data-Chunk]
    end_chunk: IEND-Chunk

type IHDR-Chunk:
    length: Uint32
    chunk_type: Bytes<4>
    width: Uint32
    height: Uint32
    bit_depth: Uint8
    color_type: Uint8
    compression: Uint8
    filter: Uint8
    interlace: Uint8
    crc: Bytes<4>

type Data-Chunk:
    length: Uint32
    chunk_type: Bytes<4>
    data: Bytes<length>
    crc: Bytes<4>

type IEND-Chunk:
    length: Uint32
    chunk_type: Bytes<4>
    crc: Bytes<4>

parse PNG-Image:
    signature = read_bytes(8)
    assert signature == b"\x89PNG\r\n\x1a\n"
    
    ihdr_chunk = parse IHDR-Chunk
    assert ihdr_chunk.chunk_type == b"IHDR"
    
    data_chunks = []
    while true:
        chunk = parse Data-Chunk
        if chunk.chunk_type == b"IEND":
            break
        data_chunks.append(chunk)
    
    end_chunk = parse IEND-Chunk
    assert end_chunk.chunk_type == b"IEND"

    return PNG-Image(
        signature: signature,
        ihdr_chunk: ihdr_chunk,
        data_chunks: data_chunks,
        end_chunk: end_chunk
    )