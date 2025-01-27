let JPEG = sequence {
    SOI: 0xFFD8,
    segments: repeat {
        segment: Segment
    },
    EOI: 0xFFD9
};

let Segment = sequence {
    marker: Marker,
    length: be_u16,
    data: switch (marker) {
        0xFFC0 => SOF0,
        0xFFC4 => DHT,
        0xFFDB => DQT,
        0xFFDD => DRI,
        0xFFDA => SOS,
        _ => GenericSegment
    }
};

let SOF0 = sequence {
    precision: u8,
    height: be_u16,
    width: be_u16,
    components: u8,
    component_info: repeat (components) {
        component_id: u8,
        sampling_factors: u8,
        quantization_table: u8
    }
};

let DHT = sequence {
    table_info: u8,
    counts: repeat (16) { u8 },
    symbols: repeat (sum(counts)) { u8 }
};

let DQT = sequence {
    table_info: u8,
    quant_table: repeat (64) { u8 }
};

let DRI = sequence {
    restart_interval: be_u16
};

let SOS = sequence {
    component_count: u8,
    component_specs: repeat (component_count) {
        component_id: u8,
        huffman_table: u8
    },
    spectral_selection: u8,
    successive_approximation: u8,
    compressed_data: until (0xFF, 0x00)
};

let GenericSegment = sequence {
    data: bytes(length - 2)
};

let Marker = be_u16;