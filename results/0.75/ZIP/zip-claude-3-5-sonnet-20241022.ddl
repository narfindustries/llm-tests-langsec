def Main = ZIP

def ZIP = {
    magic: Magic;
    files: Files[];
}

def Magic = {
    magic_number: UInt16BE where magic_number == 0x504b;
}

def Files = {
    header_signature: UInt32BE where header_signature == 0x504b0304;
    version: UInt16LE;
    flags: UInt16LE;
    compression: UInt16LE;
    mod_time: UInt16LE;
    mod_date: UInt16LE;
    crc32: UInt32LE;
    compressed_size: UInt32LE;
    uncompressed_size: UInt32LE;
    filename_length: UInt16LE;
    extra_field_length: UInt16LE;
    filename: Array filename_length uint8;
    extra_field: Array extra_field_length uint8;
    compressed_data: Array compressed_size uint8;
}