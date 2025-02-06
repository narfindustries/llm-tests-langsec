JPEG {
    SOI: 0xFFD8;
    segments: Segment*;
    EOI: 0xFFD9;
}

Segment {
    marker: Marker;
    length: uint16;
    data: Data[length - 2];
}

Marker {
    value: uint8 == 0xFF;
    type: uint8;
}

Data {
    case marker.type {
        0xC0..0xCF: SOF {
            precision: uint8;
            height: uint16;
            width: uint16;
            num_components: uint8;
            components: Component[num_components];
        }
        0xC4: DHT {
            tables: HuffmanTable*;
        }
        0xDB: DQT {
            tables: QuantizationTable*;
        }
        0xDD: DRI {
            restart_interval: uint16;
        }
        0xDA: SOS {
            num_components: uint8;
            components: ScanComponent[num_components];
            spectral_selection_start: uint8;
            spectral_selection_end: uint8;
            successive_approximation: uint8;
        }
        0xE0..0xEF: APPn {
            identifier: uint8;
            application_data: uint8*;
        }
        0xFE: COM {
            comment: uint8*;
        }
        0xD0..0xD7: RSTn {
            // No data, just a marker
        }
        default: Unknown {
            // Unknown marker, just read the data
        }
    }
}

Component {
    id: uint8;
    sampling_factors: uint8;
    quantization_table_id: uint8;
}

HuffmanTable {
    table_class: uint8;
    table_id: uint8;
    code_lengths: uint8[16];
    code_values: uint8*;
}

QuantizationTable {
    precision: uint8;
    table_id: uint8;
    table_data: uint8[64] if precision == 0 else uint16[64];
}

ScanComponent {
    id: uint8;
    dc_table_id: uint8;
    ac_table_id: uint8;
}

Unknown {
    data: uint8*;
}