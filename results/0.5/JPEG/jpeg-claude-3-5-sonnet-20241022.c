#include <hammer/hammer.h>
#include <stdio.h>

static HParser* init_jpeg_parser() {
    // Markers
    H_RULE(soi, h_token((uint8_t*)"\xFF\xD8", 2));  // Start of Image
    H_RULE(eoi, h_token((uint8_t*)"\xFF\xD9", 2));  // End of Image
    H_RULE(app0, h_token((uint8_t*)"\xFF\xE0", 2)); // APP0 marker
    H_RULE(dqt, h_token((uint8_t*)"\xFF\xDB", 2));  // DQT marker
    H_RULE(sof0, h_token((uint8_t*)"\xFF\xC0", 2)); // SOF0 marker
    H_RULE(dht, h_token((uint8_t*)"\xFF\xC4", 2));  // DHT marker
    H_RULE(sos, h_token((uint8_t*)"\xFF\xDA", 2));  // SOS marker

    // Length fields (2 bytes)
    H_RULE(length, h_uint16());

    // JFIF header components
    H_RULE(jfif_id, h_token((uint8_t*)"JFIF\0", 5));
    H_RULE(version, h_sequence(h_uint8(), h_uint8(), NULL));
    H_RULE(units, h_uint8());
    H_RULE(density, h_sequence(h_uint16(), h_uint16(), NULL));
    H_RULE(thumbnail, h_sequence(h_uint8(), h_uint8(), NULL));

    // APP0 segment
    H_RULE(app0_segment, h_sequence(app0, length, jfif_id, version, units, density, thumbnail, NULL));

    // DQT components
    H_RULE(dqt_header, h_uint8());
    H_RULE(dqt_table, h_repeat_n(h_uint8(), 64));
    H_RULE(dqt_segment, h_sequence(dqt, length, dqt_header, dqt_table, NULL));

    // SOF0 components
    H_RULE(precision, h_uint8());
    H_RULE(height, h_uint16());
    H_RULE(width, h_uint16());
    H_RULE(components_count, h_uint8());
    H_RULE(component_info, h_repeat_n(h_sequence(h_uint8(), h_uint8(), h_uint8(), NULL), 3));
    H_RULE(sof0_segment, h_sequence(sof0, length, precision, height, width, components_count, component_info, NULL));

    // DHT components
    H_RULE(dht_header, h_uint8());
    H_RULE(dht_lengths, h_repeat_n(h_uint8(), 16));
    H_RULE(dht_values, h_length_value(h_sum(dht_lengths), h_uint8()));
    H_RULE(dht_segment, h_sequence(dht, length, dht_header, dht_lengths, dht_values, NULL));

    // SOS components
    H_RULE(sos_header, h_uint8());
    H_RULE(scan_components, h_repeat_n(h_sequence(h_uint8(), h_uint8(), NULL), 3));
    H_RULE(spectral_selection, h_sequence(h_uint8(), h_uint8(), NULL));
    H_RULE(approx, h_uint8());
    H_RULE(sos_segment, h_sequence(sos, length, sos_header, scan_components, spectral_selection, approx, NULL));

    // Entropy coded data
    H_RULE(entropy_data, h_many(h_uint8()));

    // Complete JPEG structure
    H_RULE(jpeg, h_sequence(
        soi,
        app0_segment,
        h_many(dqt_segment),
        sof0_segment,
        h_many(dht_segment),
        sos_segment,
        entropy_data,
        eoi,
        NULL
    ));

    return jpeg;
}

int main() {
    return 0;
}