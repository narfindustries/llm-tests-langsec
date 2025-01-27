namespace TIFF

type TiffFile = struct {
    header: TiffHeader,
    ifds: array of Ifd(header.first_ifd_offset)
}

type TiffHeader = struct {
    byte_order: ByteOrder,
    magic_number: u16,
    first_ifd_offset: u32
} {
    if (byte_order == ByteOrder::LittleEndian) {
        endian = "le"
    } else {
        endian = "be"
    }
}

enum ByteOrder : u16 {
    LittleEndian = 0x4949,
    BigEndian = 0x4D4D
}

type Ifd = struct {
    num_entries: u16,
    entries: array of IfdEntry(num_entries),
    next_ifd_offset: u32
}

type IfdEntry = struct {
    tag: u16,
    type: u16,
    count: u32,
    value_offset: u32
}