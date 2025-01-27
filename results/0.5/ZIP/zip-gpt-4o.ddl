// Daedalus specification for ZIP file format
// Specification assumes a basic understanding of the ZIP file structure

namespace zip;

struct LocalFileHeader {
    uint32 signature; // 0x04034b50
    uint16 version_needed;
    uint16 flags;
    uint16 compression_method;
    uint16 last_mod_time;
    uint16 last_mod_date;
    uint32 crc32;
    uint32 compressed_size;
    uint32 uncompressed_size;
    uint16 filename_length;
    uint16 extra_field_length;
    string filename[filename_length];
    bytes extra_field[extra_field_length];
}

struct CentralDirectoryFileHeader {
    uint32 signature; // 0x02014b50
    uint16 version_made_by;
    uint16 version_needed;
    uint16 flags;
    uint16 compression_method;
    uint16 last_mod_time;
    uint16 last_mod_date;
    uint32 crc32;
    uint32 compressed_size;
    uint32 uncompressed_size;
    uint16 filename_length;
    uint16 extra_field_length;
    uint16 file_comment_length;
    uint16 disk_number_start;
    uint16 internal_file_attributes;
    uint32 external_file_attributes;
    uint32 relative_offset_of_local_header;
    string filename[filename_length];
    bytes extra_field[extra_field_length];
    string file_comment[file_comment_length];
}

struct EndOfCentralDirectoryRecord {
    uint32 signature; // 0x06054b50
    uint16 number_of_this_disk;
    uint16 number_of_disk_with_start_of_central_directory;
    uint16 total_number_of_entries_in_central_directory_on_this_disk;
    uint16 total_number_of_entries_in_central_directory;
    uint32 size_of_central_directory;
    uint32 offset_of_start_of_central_directory;
    uint16 comment_length;
    string comment[comment_length];
}

struct ZipFile {
    LocalFileHeader local_file_headers[];
    CentralDirectoryFileHeader central_directory_file_headers[];
    EndOfCentralDirectoryRecord end_of_central_directory_record;
}