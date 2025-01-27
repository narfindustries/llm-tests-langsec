#include <hammer/hammer.h>

HParser *create_zip_parser() {
    // Local file header signature
    HParser *local_file_header_sig = h_sequence(
        h_uint32(0x04034b50), // Local file header signature
        NULL
    );

    // Version needed to extract
    HParser *version_needed = h_uint16();

    // General purpose bit flag
    HParser *general_purpose_bit_flag = h_uint16();

    // Compression method
    HParser *compression_method = h_uint16();

    // File last modification time
    HParser *last_mod_time = h_uint16();

    // File last modification date
    HParser *last_mod_date = h_uint16();

    // CRC-32
    HParser *crc32 = h_uint32();

    // Compressed size
    HParser *compressed_size = h_uint32();

    // Uncompressed size
    HParser *uncompressed_size = h_uint32();

    // File name length
    HParser *file_name_length = h_uint16();

    // Extra field length
    HParser *extra_field_length = h_uint16();

    // File name
    HParser *file_name = h_length_value(file_name_length, h_bytes_p(1));

    // Extra field
    HParser *extra_field = h_length_value(extra_field_length, h_bytes_p(1));

    // Local file header
    HParser *local_file_header = h_sequence(
        local_file_header_sig,
        version_needed,
        general_purpose_bit_flag,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name_length,
        extra_field_length,
        file_name,
        extra_field,
        NULL
    );

    // Central directory file header signature
    HParser *central_dir_file_header_sig = h_sequence(
        h_uint32(0x02014b50), // Central directory file header signature
        NULL
    );

    // Version made by
    HParser *version_made_by = h_uint16();

    // Central directory file header
    HParser *central_dir_file_header = h_sequence(
        central_dir_file_header_sig,
        version_made_by,
        version_needed,
        general_purpose_bit_flag,
        compression_method,
        last_mod_time,
        last_mod_date,
        crc32,
        compressed_size,
        uncompressed_size,
        file_name_length,
        extra_field_length,
        file_name,
        extra_field,
        NULL
    );

    // End of central directory signature
    HParser *end_of_central_dir_sig = h_sequence(
        h_uint32(0x06054b50), // End of central directory signature
        NULL
    );

    // Number of this disk
    HParser *number_of_this_disk = h_uint16();

    // Number of the disk with the start of the central directory
    HParser *number_of_disk_with_start = h_uint16();

    // Total number of entries in the central directory on this disk
    HParser *total_entries_on_this_disk = h_uint16();

    // Total number of entries in the central directory
    HParser *total_entries = h_uint16();

    // Size of the central directory
    HParser *size_of_central_dir = h_uint32();

    // Offset of start of central directory with respect to the starting disk number
    HParser *offset_of_start_of_central_dir = h_uint32();

    // ZIP file comment length
    HParser *zip_file_comment_length = h_uint16();

    // ZIP file comment
    HParser *zip_file_comment = h_length_value(zip_file_comment_length, h_bytes_p(1));

    // End of central directory record
    HParser *end_of_central_dir = h_sequence(
        end_of_central_dir_sig,
        number_of_this_disk,
        number_of_disk_with_start,
        total_entries_on_this_disk,
        total_entries,
        size_of_central_dir,
        offset_of_start_of_central_dir,
        zip_file_comment_length,
        zip_file_comment,
        NULL
    );

    // ZIP file parser
    HParser *zip_parser = h_many(
        h_choice(
            local_file_header,
            central_dir_file_header,
            end_of_central_dir,
            NULL
        )
    );

    return zip_parser;
}