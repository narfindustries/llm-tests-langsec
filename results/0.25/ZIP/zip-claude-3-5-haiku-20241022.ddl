format ZIP:
    let magic_number = bytes("PK\x03\x04")
    let end_of_central_directory_signature = bytes("PK\x05\x06")

    type FileHeader:
        version_needed_to_extract: u16
        general_purpose_bit_flag: u16
        compression_method: u16
        last_mod_time: u16
        last_mod_date: u16
        crc32: u32
        compressed_size: u32
        uncompressed_size: u32
        file_name_length: u16
        extra_field_length: u16
        file_name: bytes(file_name_length)
        extra_field: bytes(extra_field_length)

    type CentralDirectoryFileHeader:
        version_made_by: u16
        version_needed_to_extract: u16
        general_purpose_bit_flag: u16
        compression_method: u16
        last_mod_time: u16
        last_mod_date: u16
        crc32: u32
        compressed_size: u32
        uncompressed_size: u32
        file_name_length: u16
        extra_field_length: u16
        file_comment_length: u16
        disk_number_start: u16
        internal_file_attributes: u16
        external_file_attributes: u32
        local_header_offset: u32
        file_name: bytes(file_name_length)
        extra_field: bytes(extra_field_length)
        file_comment: bytes(file_comment_length)

    type EndOfCentralDirectory:
        number_of_this_disk: u16
        disk_where_central_directory_starts: u16
        total_entries_on_this_disk: u16
        total_entries_in_central_directory: u16
        size_of_central_directory: u32
        central_directory_offset: u32
        comment_length: u16
        comment: bytes(comment_length)

    type ZIPFile:
        magic: magic_number
        file_headers: list(FileHeader)
        central_directory_headers: list(CentralDirectoryFileHeader)
        end_of_central_directory: EndOfCentralDirectory