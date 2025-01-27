typedef JPEG {
    header: SOI,
    segments: {
        marker: APP0,
        length: u16,
        identifier: 'JFIF\0',
        version: u16,
        units: u8,
        x_density: u16,
        y_density: u16,
        thumbnail_x: u8,
        thumbnail_y: u8,
        thumbnail_data: if (thumbnail_x > 0 && thumbnail_y > 0) 
            { rgb_pixels: RGB[thumbnail_x * thumbnail_y] } 
            else { },
    }*,
    segments: {
        marker: SOF0,
        length: u16,
        precision: u8,
        height: u16,
        width: u16,
        components: {
            id: u8,
            sampling_factor: u8,
            qtable_index: u8
        }+
    },
    segments: {
        marker: DHT,
        length: u16,
        huffman_table: {
            type: u8,
            lengths: u8[16],
            values: u8[sum(lengths)]
        }+
    },
    segments: {
        marker: DQT,
        length: u16,
        quantization_table: {
            precision: u8,
            table_data: u8[64]
        }+
    },
    segments: {
        marker: SOS,
        length: u16,
        component_spec: {
            component_id: u8,
            huffman_tables: u8
        }+,
        spectral_selection_start: u8,
        spectral_selection_end: u8,
        approx_high: u8,
        approx_low: u8,
        compressed_data: u8*
    },
    trailer: EOI
}