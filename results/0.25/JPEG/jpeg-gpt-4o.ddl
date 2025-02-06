Parser JPEG {
    SOI: u16be == 0xFFD8;

    segments: list of SEGMENT until EOI;

    EOI: u16be == 0xFFD9;
}

segment SEGMENT {
    marker: u16be;
    length: u16be if marker not in [0xFFD8, 0xFFD9, 0xFF01];

    body: switch(marker) {
        case 0xFFE0..0xFFEF: APPN_SEGMENT(length - 2),
        case 0xFFDB: DQT_SEGMENT(length - 2),
        case 0xFFC0..0xFFC3: SOF_SEGMENT(length - 2),
        case 0xFFC4: DHT_SEGMENT(length - 2),
        case 0xFFDA: SOS_SEGMENT(length - 2),
        case 0xFFDD: DRI_SEGMENT(length - 2),
        case 0xFFFE: COM_SEGMENT(length - 2),
        default: UNKNOWN_SEGMENT(length - 2)
    };
}

segment APPN_SEGMENT(length: u16) {
    data: bytes[length];
}

segment DQT_SEGMENT(length: u16) {
    tables: list of DQT_TABLE until length <= 0;
}

segment DQT_TABLE {
    info: u8;
    precision: u4 = info >> 4;
    identifier: u4 = info & 0x0F;
    data: bytes[precision == 0 ? 64 : 128];
}

segment SOF_SEGMENT(length: u16) {
    precision: u8;
    height: u16be;
    width: u16be;
    num_components: u8;
    components: list of SOF_COMPONENT[num_components];
}

segment SOF_COMPONENT {
    id: u8;
    sampling_factors: u8;
    quantization_table_number: u8;
}

segment DHT_SEGMENT(length: u16) {
    tables: list of DHT_TABLE until length <= 0;
}

segment DHT_TABLE {
    info: u8;
    class: u4 = info >> 4;
    identifier: u4 = info & 0x0F;
    num_codes: bytes[16];
    values: bytes[sum(num_codes)];
}

segment SOS_SEGMENT(length: u16) {
    num_components: u8;
    components: list of SOS_COMPONENT[num_components];
    start_spectral: u8;
    end_spectral: u8;
    approx: u8;
    data: bytes[length - 2 - num_components * 2 - 3];
}

segment SOS_COMPONENT {
    id: u8;
    huffman_tables: u8;
}

segment DRI_SEGMENT(length: u16) {
    restart_interval: u16be;
}

segment COM_SEGMENT(length: u16) {
    comment: bytes[length];
}

segment UNKNOWN_SEGMENT(length: u16) {
    data: bytes[length];
}