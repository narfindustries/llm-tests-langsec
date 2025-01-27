struct ZIPFile {
    signature: uint32le = 0x04034b50;
    version: uint16le;
    flags: uint16le;
    compression_method: uint16le;
    last_mod_time: uint16le;
    last_mod_date: uint16le;
    crc32: uint32le;
    compressed_size: uint32le;
    uncompressed_size: uint32le;
    file_name_length: uint16le;
    extra_field_length: uint16le;
    file_name: string(file_name_length);
    extra_field: bytes(extra_field_length);
    data: bytes(compressed_size);
}

struct CentralDirectoryHeader {
    signature: uint32le = 0x02014b50;
    version_made_by: uint16le;
    version_needed: uint16le;
    flags: uint16le;
    compression_method: uint16le;
    last_mod_time: uint16le;
    last_mod_date: uint16le;
    crc32: uint32le;
    compressed_size: uint32le;
    uncompressed_size: uint32le;
    file_name_length: uint16le;
    extra_field_length: uint16le;
    file_comment_length: uint16le;
    disk_number_start: uint16le;
    internal_attributes: uint16le;
    external_attributes: uint32le;
    local_header_offset: uint32le;
    file_name: string(file_name_length);
    extra_field: bytes(extra_field_length);
    file_comment: string(file_comment_length);
}

struct EndOfCentralDirectoryRecord {
    signature: uint32le = 0x06054b50;
    disk_number: uint16le;
    central_directory_disk_number: uint16le;
    num_central_directory_records_on_disk: uint16le;
    total_central_directory_records: uint16le;
    central_directory_size: uint32le;
    central_directory_offset: uint32le;
    comment_length: uint16le;
    comment: string(comment_length);
}

struct ZIPArchive {
    files: ZIPFile[];
    central_directory: CentralDirectoryHeader[];
    end_of_central_directory: EndOfCentralDirectoryRecord;
}