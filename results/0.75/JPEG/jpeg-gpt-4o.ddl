jpeg {
    SOI: u16be == 0xFFD8,

    segments: []segment until (segments[-1].marker == 0xFFD9),

    struct segment {
        marker: u16be,
        body: switch (marker) {
            0xFFE0..0xFFEF -> appn_segment,
            0xFFDB -> dqt_segment,
            0xFFC0 -> sof0_segment,
            0xFFC4 -> dht_segment,
            0xFFDA -> sos_segment,
            0xFFD9 -> eoi,
            _ -> skip_segment
        }
    },

    struct appn_segment {
        length: u16be,
        data: bytes[length - 2]
    },

    struct dqt_segment {
        length: u16be,
        tables: []quantization_table until (remaining_bytes() == 0)
    },

    struct quantization_table {
        table_info: u8,
        table_data: bytes[64]
    },

    struct sof0_segment {
        length: u16be,
        precision: u8,
        height: u16be,
        width: u16be,
        num_components: u8,
        components: []component_spec count(num_components)
    },

    struct component_spec {
        component_id: u8,
        sampling_factors: u8,
        quantization_table_id: u8
    },

    struct dht_segment {
        length: u16be,
        tables: []huffman_table until (remaining_bytes() == 0)
    },

    struct huffman_table {
        table_info: u8,
        code_lengths: bytes[16],
        huffman_values: bytes[calculated_huffman_size]
    },

    struct sos_segment {
        length: u16be,
        num_components: u8,
        components: []scan_component_spec count(num_components),
        spectral_start: u8,
        spectral_end: u8,
        successive_approximation: u8,
        scan_data: bytes[remaining_bytes()]
    },

    struct scan_component_spec {
        component_id: u8,
        huffman_table_ids: u8
    },

    struct eoi {
    },

    struct skip_segment {
        length: u16be,
        data: bytes[length - 2]
    }
}