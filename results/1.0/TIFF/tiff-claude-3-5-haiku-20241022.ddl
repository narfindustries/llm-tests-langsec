format TIFF {
    header = {
        byte_order : enum {
            little_endian = 0x4949,
            big_endian = 0x4D4D
        },
        version : u16,
        first_ifd_offset : u32
    }

    ifd_entry = {
        tag : u16,
        field_type : u16,
        count : u32,
        value_or_offset : u32
    }

    ifd = {
        num_entries : u16,
        entries : ifd_entry[num_entries],
        next_ifd_offset : u32
    }

    image_data_raw = bytes

    file = {
        header,
        primary_ifd : ifd,
        image_data : image_data_raw
    }

    constraints {
        header.byte_order == byte_order.little_endian,
        header.version == 42,
        sizeof(primary_ifd.entries) == primary_ifd.num_entries
    }
}