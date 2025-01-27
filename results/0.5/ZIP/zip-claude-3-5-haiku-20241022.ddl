format ZIP:
    endian big
    
    struct ZipHeader:
        signature: u32
        version: u16
        flags: u16
        compression_method: u16
        last_mod_time: u16
        last_mod_date: u16
        crc32: u32
        compressed_size: u32
        uncompressed_size: u32
        filename_length: u16
        extra_field_length: u16
    
    struct ZipFile:
        header: ZipHeader
        filename: bytes(header.filename_length)
        extra_field: bytes(header.extra_field_length)
        compressed_data: bytes(header.compressed_size)
    
    struct ZipArchive:
        files: list<ZipFile>
        
    parse(data: bytes): ZipArchive:
        archive = ZipArchive()
        offset = 0
        
        while offset < len(data):
            file = ZipFile()
            file.header = parse_struct<ZipHeader>(data[offset:])
            offset += sizeof(ZipHeader)
            
            file.filename = data[offset:offset+file.header.filename_length]
            offset += file.header.filename_length
            
            file.extra_field = data[offset:offset+file.header.extra_field_length]
            offset += file.header.extra_field_length
            
            file.compressed_data = data[offset:offset+file.header.compressed_size]
            offset += file.header.compressed_size
            
            archive.files.append(file)
        
        return archive