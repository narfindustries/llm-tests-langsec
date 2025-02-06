format jpeg {
    let SOI = 0xFFD8;
    let APP0 = 0xFFE0;
    let APP1 = 0xFFE1;
    let DQT = 0xFFDB;
    let DHT = 0xFFC4;
    let SOF0 = 0xFFC0;
    let SOF2 = 0xFFC2;
    let SOS = 0xFFDA;
    let EOI = 0xFFD9;

    record Jpeg {
        magic: u16 = SOI,
        segments: Segment[] while last.marker != EOI
    }

    variant Segment {
        Soi: {
            marker: u16 = SOI
        },
        App0: {
            marker: u16 = APP0,
            length: u16,
            identifier: string(5),
            version: u16,
            density_units: u8,
            x_density: u16,
            y_density: u16,
            thumbnail_width: u8,
            thumbnail_height: u8,
            thumbnail_data: u8[thumbnail_width * thumbnail_height * 3]?
        },
        App1: {
            marker: u16 = APP1,
            length: u16,
            identifier: string,
            data: u8[]
        },
        Dqt: {
            marker: u16 = DQT,
            length: u16,
            precision: u8,
            table_destination: u8,
            quantization_values: u8[64]
        },
        Dht: {
            marker: u16 = DHT,
            length: u16,
            table_class: u8,
            destination_id: u8,
            huffman_codes: u8[]
        },
        Sof: {
            marker: u16 = SOF0 | SOF2,
            length: u16,
            precision: u8,
            height: u16,
            width: u16,
            components: u8,
            component_data: ComponentData[components]
        },
        Sos: {
            marker: u16 = SOS,
            length: u16,
            num_components: u8,
            component_selectors: ComponentSelector[num_components],
            spectral_start: u8,
            spectral_end: u8,
            successive_approximation: u8,
            compressed_data: u8[]
        },
        Eoi: {
            marker: u16 = EOI
        }
    }

    record ComponentData {
        component_id: u8,
        sampling_factors: u8,
        quantization_table: u8
    }

    record ComponentSelector {
        component_id: u8,
        dc_huffman_table: u8,
        ac_huffman_table: u8
    }
}