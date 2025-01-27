module zip-gpt-4o {
    struct LocalFileHeader {
        uint32 signature;                 // 0x04034b50
        uint16 version_needed;
        uint16 flags;
        uint16 compression;
        uint16 mod_time;
        uint16 mod_date;
        uint32 crc32;
        uint32 compressed_size;
        uint32 uncompressed_size;
        uint16 filename_length;
        uint16 extra_field_length;
        char[filename_length] filename;
        char[extra_field_length] extra_field;
        bytes[compressed_size] compressed_data;
    }

    struct CentralDirectoryFileHeader {
        uint32 signature;                 // 0x02014b50
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
        uint16 extra_field_length;
        uint16 file_comment_length;
        uint16 disk_number_start;
        uint16 internal_file_attributes;
        uint32 external_file_attributes;
        uint32 local_header_offset;
        char[filename_length] filename;
        char[extra_field_length] extra_field;
        char[file_comment_length] file_comment;
    }

    struct EndOfCentralDirectoryRecord {
        uint32 signature;                 // 0x06054b50
        uint16 disk_number;
        uint16 central_directory_disk_number;
        uint16 num_records_on_this_disk;
        uint16 total_num_records;
        uint32 central_directory_size;
        uint32 central_directory_offset;
        uint16 zip_file_comment_length;
        char[zip_file_comment_length] zip_file_comment;
    }

    struct ZipFile {
        iterate<LocalFileHeader> local_file_headers until eof;
        iterate<CentralDirectoryFileHeader> central_directory_file_headers until matching_signature(0x06054b50);
        EndOfCentralDirectoryRecord end_of_central_directory_record;
    }

    function matching_signature(expected_signature) {
        return peek_ahead(uint32) == expected_signature;
    }
}