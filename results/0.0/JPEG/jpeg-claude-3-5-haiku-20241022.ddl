def jpeg = {
    magic: 0xFFD8;
    segments: segment+;
    end_marker: 0xFFD9;
}

def segment = {
    marker: u16;
    match marker {
        0xFFE0 => app0_segment,
        0xFFE1 => app1_segment,
        0xFFDB => dqt_segment,
        0xFFC4 => dht_segment,
        0xFFC0 => sof_baseline_segment,
        0xFFC2 => sof_progressive_segment,
        0xFFDA => sos_segment,
        _ => unknown_segment
    }
}

def app0_segment = {
    length: u16;
    identifier: string(5);
    version: u16;
    units: u8;
    x_density: u16;
    y_density: u16;
    thumbnail_x: u8;
    thumbnail_y: u8;
    thumbnail_data: bytes(thumbnail_x * thumbnail_y * 3)
}

def app1_segment = {
    length: u16;
    exif_data: bytes(length - 2)
}

def dqt_segment = {
    length: u16;
    tables: quantization_table+
}

def quantization_table = {
    precision: u8 >> 4;
    table_id: u8 & 0x0F;
    values: bytes(precision == 0 ? 64 : 128)
}

def dht_segment = {
    length: u16;
    tables: huffman_table+
}

def huffman_table = {
    table_class: u8 >> 4;
    table_dest: u8 & 0x0F;
    lengths: u8[16];
    values: bytes(sum(lengths))
}

def sof_baseline_segment = {
    length: u16;
    precision: u8;
    height: u16;
    width: u16;
    components: u8;
    component_data: component_info[components]
}

def component_info = {
    id: u8;
    sampling_factors: u8;
    quantization_table_id: u8
}

def sof_progressive_segment = sof_baseline_segment

def sos_segment = {
    length: u16;
    num_components: u8;
    component_selectors: component_selector[num_components];
    spectral_start: u8;
    spectral_end: u8;
    approx_high: u4;
    approx_low: u4;
    compressed_data: bytes
}

def component_selector = {
    component_id: u8;
    dc_ac_tables: u8
}

def unknown_segment = {
    length: u16;
    data: bytes(length - 2)
}