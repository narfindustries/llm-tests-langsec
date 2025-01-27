little-endian

struct ZipFile {
    local uint32 signature = 0x04034b50; // Local file header signature
    uint16 version;
    uint16 flags;
    uint16 compression;
    uint16 mod_time;
    uint16 mod_date;
    uint32 crc32;
    uint32 compressed_size;
    uint32 uncompressed_size;
    uint16 filename_length;
    uint16 extra_length;
    string filename[filename_length];
    bytes extra[extra_length];
    bytes compressed_data[compressed_size];
}

struct CentralDirectoryFileHeader {
    local uint32 signature = 0x02014b50; // Central directory file header signature
    uint16 version_made_by;
    uint16 version_needed;
    uint16 flags;
    uint16 compression;
    uint16 mod_time;
    uint16 mod_date;
    uint32 crc32;
    uint32 compressed_size;
    uint32 uncompressed_size;
    uint16 filename_length;
    uint16 extra_length;
    uint16 comment_length;
    uint16 disk_number_start;
    uint16 internal_attributes;
    uint32 external_attributes;
    uint32 local_header_offset;
    string filename[filename_length];
    bytes extra[extra_length];
    string comment[comment_length];
}

struct EndOfCentralDirectoryRecord {
    local uint32 signature = 0x06054b50; // End of central directory signature
    uint16 disk_number;
    uint16 central_directory_disk_number;
    uint16 central_directory_records_on_this_disk;
    uint16 total_central_directory_records;
    uint32 central_directory_size;
    uint32 central_directory_offset;
    uint16 comment_length;
    string comment[comment_length];
}

struct ZipArchive {
    ZipFile files[];
    CentralDirectoryFileHeader cd_headers[];
    EndOfCentralDirectoryRecord eocd;
}