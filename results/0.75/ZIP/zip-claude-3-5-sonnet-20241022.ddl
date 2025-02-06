def ZipFile = {
    local_headers: Vector<LocalFileHeader>,
    central_directory: Vector<CentralDirectoryHeader>,
    end_of_central_directory: EndOfCentralDirectory,
    zip64_end_of_central_directory: Option<Zip64EndOfCentralDirectory>,
    zip64_end_of_central_directory_locator: Option<Zip64EndOfCentralDirectoryLocator>
}

def LocalFileHeader = {
    signature: u32 where signature == 0x04034b50,
    version_needed: u16,
    flags: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_name: bytes(file_name_length),
    extra_field: bytes(extra_field_length),
    compressed_data: bytes(compressed_size)
}

def CentralDirectoryHeader = {
    signature: u32 where signature == 0x02014b50,
    version_made_by: u16,
    version_needed: u16,
    flags: u16,
    compression_method: u16,
    last_mod_time: u16,
    last_mod_date: u16,
    crc32: u32,
    compressed_size: u32,
    uncompressed_size: u32,
    file_name_length: u16,
    extra_field_length: u16,
    file_comment_length: u16,
    disk_number_start: u16,
    internal_file_attributes: u16,
    external_file_attributes: u32,
    local_header_offset: u32,
    file_name: bytes(file_name_length),
    extra_field: bytes(extra_field_length),
    file_comment: bytes(file_comment_length)
}

def EndOfCentralDirectory = {
    signature: u32 where signature == 0x06054b50,
    disk_number: u16,
    central_dir_disk_number: u16,
    disk_entries: u16,
    total_entries: u16,
    central_dir_size: u32,
    central_dir_offset: u32,
    comment_length: u16,
    comment: bytes(comment_length)
}

def Zip64EndOfCentralDirectory = {
    signature: u32 where signature == 0x06064b50,
    record_size: u64,
    version_made_by: u16,
    version_needed: u16,
    disk_number: u32,
    central_dir_disk_number: u32,
    disk_entries: u64,
    total_entries: u64,
    central_dir_size: u64,
    central_dir_offset: u64,
    extensible_data: bytes(record_size - 44)
}

def Zip64EndOfCentralDirectoryLocator = {
    signature: u32 where signature == 0x07064b50,
    disk_number: u32,
    relative_offset: u64,
    total_disks: u32
}

def CompressionMethod = enum {
    Store = 0,
    Shrunk = 1,
    Reduced1 = 2,
    Reduced2 = 3,
    Reduced3 = 4,
    Reduced4 = 5,
    Imploded = 6,
    Deflated = 8,
    EnhancedDeflated = 9,
    PKWareDCLImploded = 10,
    BZIP2 = 12,
    LZMA = 14,
    XZ = 95,
    JPEG = 96,
    WavPack = 97,
    PPMd = 98
}

def GeneralPurposeBitFlag = flags {
    Encrypted = 0,
    CompressionOption1 = 1,
    CompressionOption2 = 2,
    DataDescriptor = 3,
    EnhancedDeflation = 4,
    CompressedPatched = 5,
    StrongEncryption = 6,
    Unused7 = 7,
    Unused8 = 8,
    Unused9 = 9,
    Unused10 = 10,
    LanguageEncoding = 11,
    Reserved12 = 12,
    MaskHeaderValues = 13,
    Reserved14 = 14,
    Reserved15 = 15
}