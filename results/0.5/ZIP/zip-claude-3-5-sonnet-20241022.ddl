struct ZIP {
    LocalFileHeader[] files;
    CentralDirectoryHeader[] central_directory;
    ZIP64_EndOfCentralDirectory? zip64_end_of_central_directory;
    ZIP64_EndOfCentralDirectoryLocator? zip64_end_of_central_directory_locator;
    EndOfCentralDirectory end_of_central_directory;
}

struct LocalFileHeader {
    u32 signature = 0x04034b50;
    u16 version_needed;
    u16 flags;
    u16 compression_method;
    u16 last_mod_time;
    u16 last_mod_date;
    u32 crc32;
    u32 compressed_size;
    u32 uncompressed_size;
    u16 filename_length;
    u16 extra_field_length;
    bytes(filename_length) filename;
    bytes(extra_field_length) extra_field;
    bytes(compressed_size) data;
    DataDescriptor? data_descriptor if (flags & 0x0008) != 0;
}

struct DataDescriptor {
    u32 crc32;
    u32 compressed_size;
    u32 uncompressed_size;
}

struct CentralDirectoryHeader {
    u32 signature = 0x02014b50;
    u16 version_made_by;
    u16 version_needed;
    u16 flags;
    u16 compression_method;
    u16 last_mod_time;
    u16 last_mod_date;
    u32 crc32;
    u32 compressed_size;
    u32 uncompressed_size;
    u16 filename_length;
    u16 extra_field_length;
    u16 file_comment_length;
    u16 disk_number_start;
    u16 internal_attrs;
    u32 external_attrs;
    u32 local_header_offset;
    bytes(filename_length) filename;
    bytes(extra_field_length) extra_field;
    bytes(file_comment_length) file_comment;
}

struct EndOfCentralDirectory {
    u32 signature = 0x06054b50;
    u16 disk_number;
    u16 start_disk_number;
    u16 entries_on_disk;
    u16 total_entries;
    u32 central_directory_size;
    u32 central_directory_offset;
    u16 comment_length;
    bytes(comment_length) comment;
}

struct ZIP64_EndOfCentralDirectory {
    u32 signature = 0x06064b50;
    u64 record_size;
    u16 version_made_by;
    u16 version_needed;
    u32 disk_number;
    u32 start_disk_number;
    u64 entries_on_disk;
    u64 total_entries;
    u64 central_directory_size;
    u64 central_directory_offset;
    bytes(record_size - 44) extensible_data;
}

struct ZIP64_EndOfCentralDirectoryLocator {
    u32 signature = 0x07064b50;
    u32 disk_with_central_directory;
    u64 end_of_central_directory_offset;
    u32 total_disks;
}

enum CompressionMethod : u16 {
    NONE = 0,
    SHRUNK = 1,
    REDUCED_1 = 2,
    REDUCED_2 = 3,
    REDUCED_3 = 4,
    REDUCED_4 = 5,
    IMPLODED = 6,
    DEFLATED = 8,
    ENHANCED_DEFLATED = 9,
    PKWARE_DCL_IMPLODED = 10,
    BZIP2 = 12,
    LZMA = 14,
    IBM_TERSE = 18,
    IBM_LZ77 = 19
}

bitfield GeneralPurposeBitFlag : u16 {
    encrypted : 1;
    compression_option1 : 1;
    compression_option2 : 1;
    data_descriptor : 1;
    enhanced_deflation : 1;
    compressed_patched : 1;
    strong_encryption : 1;
    unused1 : 4;
    language_encoding : 1;
    reserved : 1;
    mask_header_values : 1;
    unused2 : 2;
}

enum VersionMadeBy : u8 {
    MS_DOS = 0,
    AMIGA = 1,
    OPEN_VMS = 2,
    UNIX = 3,
    VM_CMS = 4,
    ATARI_ST = 5,
    OS2_HPFS = 6,
    MACINTOSH = 7,
    Z_SYSTEM = 8,
    CP_M = 9,
    WINDOWS_NTFS = 10,
    MVS = 11,
    VSE = 12,
    ACORN_RISC = 13,
    VFAT = 14,
    ALTERNATE_MVS = 15,
    BEOS = 16,
    TANDEM = 17,
    OS_400 = 18,
    OS_X = 19
}