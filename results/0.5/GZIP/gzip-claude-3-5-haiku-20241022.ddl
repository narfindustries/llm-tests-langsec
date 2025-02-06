type GzipFile = struct {
    magic: [2]u8 where magic[0] == 0x1F and magic[1] == 0x8B,
    compression_method: u8 where compression_method == 0x08,
    flags: Flags,
    mtime: u32,
    extra_flags: u8,
    os_type: u8,
    optional_fields: OptionalFields(flags),
    compressed_data: bytes,
    crc32: u32,
    original_length: u32
}

type Flags = struct {
    is_text: bool,
    has_crc: bool,
    has_extra: bool,
    has_filename: bool,
    has_comment: bool,
    is_encrypted: bool,
    reserved1: bool,
    reserved2: bool
}

type OptionalFields = struct {
    extra_data: if flags.has_extra then ExtraData else unit,
    filename: if flags.has_filename then string(null_terminated) else unit,
    comment: if flags.has_comment then string(null_terminated) else unit,
    header_crc: if flags.has_crc then u16 else unit
}

type ExtraData = struct {
    length: u16,
    data: bytes(length)
}