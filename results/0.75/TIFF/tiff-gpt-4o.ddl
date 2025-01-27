define tiff_file {
    header: header,
    ifd_records: ifd_record[header.ifd_count]
}

define header {
    byte_order: u2,
    version: u2,
    ifd_offset: u4
}

define ifd_record {
    tag: u2,
    type: u2,
    count: u4,
    value_offset: u4
}

define byte_order {
    le: "II",
    be: "MM"
}

constraints {
    header.version == 42,
    (header.byte_order == byte_order.le) or (header.byte_order == byte_order.be)
} 