format Zip:
    let Magic = 0x04034b50 as u32
    let Version = 0x14 as u16
    let Compression = 0x00 as u16

    type Header:
        magic: u32
        version: u16
        flags: u16
        compression: u16
        last_mod_time: u16
        last_mod_date: u16
        crc32: u32
        compressed_size: u32
        uncompressed_size: u32
        filename_length: u16
        extra_field_length: u16

    type File:
        header: Header
        filename: [u8; header.filename_length]
        extra_field: [u8; header.extra_field_length]
        data: [u8; header.compressed_size]

    type Archive:
        files: [File]

    validate Header:
        magic == Magic
        version == Version
        compression == Compression

    parse Zip:
        let archive = Archive {
            files: repeat File
        }
        return archive