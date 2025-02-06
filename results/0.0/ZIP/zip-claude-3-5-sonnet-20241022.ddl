def ZIP = {
    local_headers : Local_File_Header[]
    central_headers : Central_Directory_Header[]
    end_directory : End_Of_Central_Directory
}

def Local_File_Header = {
    signature : u32 == 0x04034b50
    version_needed : u16
    flags : u16
    compression_method : u16
    last_mod_time : u16
    last_mod_date : u16
    crc32 : u32
    compressed_size : u32
    uncompressed_size : u32
    filename_length : u16
    extra_field_length : u16
    filename : bytes(filename_length)
    extra_field : bytes(extra_field_length)
    data : bytes(compressed_size)
    if (flags & 0x8) != 0 {
        data_descriptor : Data_Descriptor
    }
}

def Data_Descriptor = {
    signature : u32 == 0x08074b50
    crc32 : u32
    compressed_size : u32
    uncompressed_size : u32
}

def Central_Directory_Header = {
    signature : u32 == 0x02014b50
    version_made_by : u16
    version_needed : u16
    flags : u16
    compression_method : u16
    last_mod_time : u16
    last_mod_date : u16
    crc32 : u32
    compressed_size : u32
    uncompressed_size : u32
    filename_length : u16
    extra_field_length : u16
    comment_length : u16
    disk_number_start : u16
    internal_attrs : u16
    external_attrs : u32
    local_header_offset : u32
    filename : bytes(filename_length)
    extra_field : bytes(extra_field_length)
    comment : bytes(comment_length)
}

def End_Of_Central_Directory = {
    signature : u32 == 0x06054b50
    disk_number : u16
    central_dir_disk : u16
    disk_entries : u16
    total_entries : u16
    central_dir_size : u32
    central_dir_offset : u32
    comment_length : u16
    if comment_length > 0 {
        comment : bytes(comment_length)
    }
}

def CompressionMethod = union {
    NONE : 0
    SHRUNK : 1
    REDUCED_1 : 2
    REDUCED_2 : 3
    REDUCED_3 : 4
    REDUCED_4 : 5
    IMPLODED : 6
    DEFLATED : 8
    ENHANCED_DEFLATED : 9
    PKWARE_DCL : 10
    BZIP2 : 12
    LZMA : 14
    PPMD : 98
}

def Flags = bitfield {
    encrypted : 0
    comp_option1 : 1
    comp_option2 : 2
    data_descriptor : 3
    enhanced_deflation : 4
    compressed_patched : 5
    strong_encryption : 6
    unused1 : 7..10
    utf8 : 11
    unused2 : 12
    mask_header : 13
    unused3 : 14..15
}