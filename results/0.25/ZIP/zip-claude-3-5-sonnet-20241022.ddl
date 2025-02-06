struct LocalFileHeader {
    magic: uint32 = 0x04034b50;
    version_needed: uint16;
    flags: uint16;
    compression_method: uint16;
    last_mod_time: uint16;
    last_mod_date: uint16;
    crc32: uint32;
    compressed_size: uint32;
    uncompressed_size: uint32;
    filename_length: uint16;
    extra_field_length: uint16;
    filename: bytes(filename_length);
    extra_field: bytes(extra_field_length);
}

struct DataDescriptor {
    crc32: uint32;
    compressed_size: uint32;
    uncompressed_size: uint32;
}

struct CentralDirectoryHeader {
    magic: uint32 = 0x02014b50;
    version_made_by: uint16;
    version_needed: uint16;
    flags: uint16;
    compression_method: uint16;
    last_mod_time: uint16;
    last_mod_date: uint16;
    crc32: uint32;
    compressed_size: uint32;
    uncompressed_size: uint32;
    filename_length: uint16;
    extra_field_length: uint16;
    comment_length: uint16;
    disk_number_start: uint16;
    internal_attrs: uint16;
    external_attrs: uint32;
    local_header_offset: uint32;
    filename: bytes(filename_length);
    extra_field: bytes(extra_field_length);
    comment: bytes(comment_length);
}

struct EndOfCentralDirectory {
    magic: uint32 = 0x06054b50;
    disk_number: uint16;
    start_disk_number: uint16;
    entries_on_disk: uint16;
    total_entries: uint16;
    central_directory_size: uint32;
    central_directory_offset: uint32;
    comment_length: uint16;
    comment: bytes(comment_length);
}

struct Zip64EndOfCentralDirectory {
    magic: uint32 = 0x06064b50;
    record_size: uint64;
    version_made_by: uint16;
    version_needed: uint16;
    disk_number: uint32;
    start_disk_number: uint32;
    entries_on_disk: uint64;
    total_entries: uint64;
    central_directory_size: uint64;
    central_directory_offset: uint64;
    extensible_data: bytes(record_size - 44);
}

struct Zip64EndOfCentralDirectoryLocator {
    magic: uint32 = 0x07064b50;
    start_disk_number: uint32;
    relative_offset: uint64;
    total_disks: uint32;
}

struct ZipArchive {
    local_headers: LocalFileHeader[];
    central_directory: CentralDirectoryHeader[];
    zip64_end_of_central_directory: optional<Zip64EndOfCentralDirectory>;
    zip64_end_of_central_directory_locator: optional<Zip64EndOfCentralDirectoryLocator>;
    end_of_central_directory: EndOfCentralDirectory;
}

entrypoint ZipArchive