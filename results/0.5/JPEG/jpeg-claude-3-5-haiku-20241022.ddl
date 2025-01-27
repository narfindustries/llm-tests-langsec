jpeg_file = {
    header: jpeg_header,
    segments: jpeg_segments
}

jpeg_header = {
    start_marker: 0xFFD8,
    markers: [start_marker]
}

jpeg_segments = [
    app_segments*,
    quantization_tables,
    huffman_tables,
    start_of_frame,
    start_of_scan,
    compressed_data,
    end_marker
]

app_segments = {
    marker: 0xFFE0 to 0xFFEF,
    length: uint16,
    data: bytes(length - 2)
}

quantization_tables = {
    marker: 0xFFDB,
    length: uint16,
    tables: [quantization_table+]
}

quantization_table = {
    precision: uint8,
    table_data: bytes(64)
}

huffman_tables = {
    marker: 0xFFC4,
    length: uint16,
    tables: [huffman_table+]
}

huffman_table = {
    table_class: uint8,
    destination_id: uint8,
    lengths: uint8[16],
    values: bytes(sum(lengths))
}

start_of_frame = {
    marker: 0xFFC0,
    length: uint16,
    precision: uint8,
    height: uint16,
    width: uint16,
    components: [color_component+]
}

color_component = {
    id: uint8,
    sampling_factors: uint8,
    quantization_table_id: uint8
}

start_of_scan = {
    marker: 0xFFC0,
    length: uint16,
    component_count: uint8,
    scan_components: [scan_component+],
    spectral_selection_start: uint8,
    spectral_selection_end: uint8,
    approximation_high: uint8,
    approximation_low: uint8
}

scan_component = {
    component_id: uint8,
    dc_huffman_table: uint8,
    ac_huffman_table: uint8
}

compressed_data = {
    data: bytes
}

end_marker = {
    marker: 0xFFD9
}