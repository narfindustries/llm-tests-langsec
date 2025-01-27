namespace TIFF

// TIFF Header
struct TiffHeader {
    endian: Endian;
    magic_number: u16;
    ifd_offset: u32;
}

// TIFF Image File Directory (IFD) Entry
struct IfdEntry {
    tag: u16;
    type: u16;
    count: u32;
    value_offset: u32;
}

// TIFF Image File Directory (IFD)
struct Ifd {
    num_entries: u16;
    entries: IfdEntry[num_entries];
    next_ifd_offset: u32;
}

// TIFF File
struct TiffFile {
    header: TiffHeader;
    ifds: Ifd[];
}

// Define endianness based on magic number
enum Endian {
    LITTLE = 0x4949,
    BIG = 0x4D4D
}

// Entry point
entry TiffFile;
