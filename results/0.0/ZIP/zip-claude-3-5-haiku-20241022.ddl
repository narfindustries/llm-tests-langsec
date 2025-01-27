format ZIP:
    signature = bytes[4]
    version = u16
    flags = u16
    compression_method = u16
    last_mod_time = u16
    last_mod_date = u16
    crc32 = u32
    compressed_size = u32
    uncompressed_size = u32
    filename_length = u16
    extra_field_length = u16
    filename = bytes[filename_length]
    extra_field = bytes[extra_field_length]
    file_data = bytes[compressed_size]

    constraints:
        signature == b"PK\x03\x04"
        version >= 10
        version <= 99
        compression_method <= 8
        crc32 > 0
        compressed_size > 0
        uncompressed_size > 0
        filename_length > 0
        filename_length <= 255