format TIFF {
    endian little;

    struct Header {
        magic: u16 = 0x4949; // "II" for little-endian
        version: u16 = 42;
        ifd_offset: u32;
    }

    struct IFDEntry {
        tag: u16;
        type: u16;
        count: u32;
        value_or_offset: u32;
    }

    struct IFD {
        num_entries: u16;
        entries: IFDEntry[num_entries];
        next_ifd_offset: u32 = 0;
    }

    struct ImageData {
        width: u32;
        height: u32;
        bits_per_sample: u16 = 8;
        samples_per_pixel: u16 = 3;
        strip_offsets: u32[];
        strip_byte_counts: u32[];
        data: u8[];
    }

    let file = {
        header: Header,
        primary_ifd: IFD,
        image: ImageData
    };
}