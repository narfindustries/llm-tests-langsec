gzip = record {
    id1: u8,
    id2: u8,
    cm: u8,
    flg: u8,
    mtime: u32,
    xfl: u8,
    os: u8,
    data: bytes,
    crc32: u32,
    isize: u32,
    optional_fields: seq(optional_field),
}

optional_field = record {
    type: u16,
    data: bytes,
}

optional_field_types = {
    0x0001: extra_fields,
    0x0002: filename,
    0x0003: comment,
}

extra_fields = record {
    subfields: seq(extra_subfield),
}

extra_subfield = record {
    id: u16,
    size: u16,
    data: bytes(size),
}

filename = record {
    name: string,
}

comment = record {
    comment: string,
}

string = seq(u8)
