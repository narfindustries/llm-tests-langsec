typedef ZipFile union {
    sections: LocalFileSection[],
    central_directory: CentralDirectory,
    end_record: EndOfCentralDirectory
}

typedef LocalFileSection union {
    header: LocalFileHeader,
    data: byte[header.compressed_size]
}

typedef LocalFileHeader union {
    signature: u32 where value == 0x04034b50,
    version_needed: u16,
    flags: GeneralPurposeBitFlag,
    compression_method: CompressionMethod,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_name: byte[file_name_length],
    extra_fields: ExtraField[]
}

typedef CentralDirectory union {
    entries: CentralDirectoryFileHeader[]
}

typedef CentralDirectoryFileHeader union {
    signature: u32 where value == 0x02014b50,
    version_made_by: u16,
    version_needed: u16,
    flags: GeneralPurposeBitFlag,
    compression_method: CompressionMethod,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_attrs: u16,
    external_attrs: u32,
    local_header_offset: u32,
    file_name: byte[file_name_length],
    extra_fields: ExtraField[],
    file_comment: byte[file_comment_length]
}

typedef EndOfCentralDirectory union {
    signature: u32 where value == 0x06054b50,
    disk_number: u16,
    cd_start_disk: u16,
    cd_records_on_disk: u16,
    cd_records_total: u16,
    cd_size: u32,
    cd_offset: u32,
    comment_length: u16,
    comment: byte[comment_length]
}

typedef GeneralPurposeBitFlag bitfield<u16> {
    encrypted: 0,
    compression_option1: 1,
    compression_option2: 2,
    data_descriptor: 3,
    enhanced_deflation: 4,
    compressed_patched: 5,
    strong_encryption: 6,
    unused1: 7..10,
    utf8: 11,
    reserved1: 12,
    mask_header: 13,
    reserved2: 14..15
}

typedef CompressionMethod enum<u16> {
    STORE = 0,
    SHRUNK = 1,
    REDUCED_1 = 2,
    REDUCED_2 = 3,
    REDUCED_3 = 4,
    REDUCED_4 = 5,
    IMPLODED = 6,
    DEFLATED = 8,
    ENHANCED_DEFLATED = 9,
    PKWARE_DCL = 10,
    BZIP2 = 12,
    LZMA = 14,
    PPMD = 98
}

typedef ExtraField union {
    header_id: u16,
    length: u16,
    data: byte[length]
}

typedef Zip64ExtendedInformation union {
    header_id: u16 where value == 0x0001,
    length: u16,
    uncompressed_size: u64,
    compressed_size: u64,
    local_header_offset: u64,
    disk_start_number: u32
}

typedef NTFSExtraField union {
    header_id: u16 where value == 0x000a,
    length: u16,
    reserved: u32,
    attributes: NTFSAttribute[]
}

typedef NTFSAttribute union {
    tag: u16,
    size: u16,
    data: byte[size]
}

typedef DataDescriptor union {
    signature: u32 where value == 0x08074b50,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32
}

typedef Zip64DataDescriptor union {
    signature: u32 where value == 0x08074b50,
    crc32: u32,
    compressed_size: u64,
    uncompressed_size: u64
}