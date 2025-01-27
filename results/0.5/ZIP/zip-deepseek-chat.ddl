struct ZIPFile {
    magic: magic = "\x50\x4B\x03\x04";  // PK header
    version: uint16;
    general_purpose_bit_flag: uint16;
    compression_method: uint16;
    last_mod_file_time: uint16;
    last_mod_file_date: uint16;
    crc32: uint32;
    compressed_size: uint32;
    uncompressed_size: uint32;
    file_name_length: uint16;
    extra_field_length: uint16;
    file_name: bytes[file_name_length];
    extra_field: bytes[extra_field_length];
    file_data: bytes[compressed_size];
}

struct CentralDirectoryHeader {
    magic: magic = "\x50\x4B\x01\x02";  // Central directory header
    version_made_by: uint16;
    version_needed_to_extract: uint16;
    general_purpose_bit_flag: uint16;
    compression_method: uint16;
    last_mod_file_time: uint16;
    last_mod_file_date: uint16;
    crc32: uint32;
    compressed_size: uint32;
    uncompressed_size: uint32;
    file_name_length: uint16;
    extra_field_length: uint16;
    file_comment_length: uint16;
    disk_number_start: uint16;
    internal_file_attributes: uint16;
    external_file_attributes: uint32;
    relative_offset_of_local_header: uint32;
    file_name: bytes[file_name_length];
    extra_field: bytes[extra_field_length];
    file_comment: bytes[file_comment_length];
}

struct EndOfCentralDirectoryRecord {
    magic: magic = "\x50\x4B\x05\x06";  // End of central directory record
    disk_number: uint16;
    central_directory_disk_number: uint16;
    num_central_directory_records_on_disk: uint16;
    total_central_directory_records: uint16;
    central_directory_size: uint32;
    central_directory_offset: uint32;
    comment_length: uint16;
    comment: bytes[comment_length];
}

struct ZIPArchive {
    files: ZIPFile[];
    central_directory: CentralDirectoryHeader[];
    end_of_central_directory: EndOfCentralDirectoryRecord;
}