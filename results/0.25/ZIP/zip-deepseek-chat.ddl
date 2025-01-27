struct ZIPFile {
    magic: magic = "\x50\x4B\x03\x04";  // PK header
    version: uint16;
    flags: uint16;
    compression_method: uint16;
    last_mod_time: uint16;
    last_mod_date: uint16;
    crc32: uint32;
    compressed_size: uint32;
    uncompressed_size: uint32;
    file_name_length: uint16;
    extra_field_length: uint16;
    file_name: bytes[file_name_length];
    extra_field: bytes[extra_field_length];
    data: bytes[compressed_size];
}

struct CentralDirectoryHeader {
    magic: magic = "\x50\x4B\x01\x02";  // Central directory header
    version_made_by: uint16;
    version_needed: uint16;
    flags: uint16;
    compression_method: uint16;
    last_mod_time: uint16;
    last_mod_date: uint16;
    crc32: uint32;
    compressed_size: uint32;
    uncompressed_size: uint32;
    file_name_length: uint16;
    extra_field_length: uint16;
    file_comment_length: uint16;
    disk_number_start: uint16;
    internal_attrs: uint16;
    external_attrs: uint32;
    local_header_offset: uint32;
    file_name: bytes[file_name_length];
    extra_field: bytes[extra_field_length];
    file_comment: bytes[file_comment_length];
}

struct EndOfCentralDirectory {
    magic: magic = "\x50\x4B\x05\x06";  // End of central directory header
    disk_number: uint16;
    central_dir_disk: uint16;
    num_central_dir_records_on_disk: uint16;
    total_central_dir_records: uint16;
    central_dir_size: uint32;
    central_dir_offset: uint32;
    comment_length: uint16;
    comment: bytes[comment_length];
}

struct ZIPArchive {
    files: ZIPFile[];
    central_directory: CentralDirectoryHeader[];
    eocd: EndOfCentralDirectory;
}