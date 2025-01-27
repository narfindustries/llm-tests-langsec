format GZIP {
    let magic_number = bytes([0x1F, 0x8B]);
    let compression_method = 8; // Deflate
    let flags = 0;
    let mtime = 0;
    let extra_flags = 0;
    let os = 3; // Unix

    type Header {
        magic: magic_number,
        compression: u8 = compression_method,
        flags: u8 = flags,
        mtime: u32 = mtime,
        extra_flags: u8 = extra_flags,
        os: u8 = os
    }

    type Compressed_Block {
        is_last_block: bool,
        block_type: u2,
        block_data: bytes
    }

    type Footer {
        crc32: u32,
        input_size: u32
    }

    type File {
        header: Header,
        compressed_blocks: [Compressed_Block],
        footer: Footer
    }

    let parse = File
}