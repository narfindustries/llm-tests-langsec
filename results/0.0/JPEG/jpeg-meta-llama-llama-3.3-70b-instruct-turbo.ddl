seq jpeg_file {
  uint16 magic = 0xFFD8,
  soi: StartOfImage,
  tables: array[DefineQuantizationTable | DefineHuffmanTable | DefineArithmeticCodingConditioning | DefineProgressiveHuffmanTable | DefineLosslessHuffmanTable],
  sos: StartOfScan,
  entropy_coded_data: array[uint8],
  eoi: EndOfImage,
}

seq StartOfImage {
  uint16 marker = 0xFFD8,
  uint16 marker_length = 2,
  uint8 precision,
  uint16 height,
  uint16 width,
  uint8 num_components,
  components: array[Component],
}

seq Component {
  uint8 id,
  uint8 horizontal_sampling_factor,
  uint8 vertical_sampling_factor,
  uint8 quantization_table_id,
}

seq DefineQuantizationTable {
  uint16 marker = 0xFFDB,
  uint16 marker_length,
  uint8 quantization_table_id,
  uint8 precision,
  quantization_table: array[uint8],
}

seq DefineHuffmanTable {
  uint16 marker = 0xFFC4,
  uint16 marker_length,
  uint8 huffman_table_id,
  uint8 class,
  uint8 num_codes,
  code_lengths: array[uint8],
  huffman_codes: array[uint8],
}

seq DefineArithmeticCodingConditioning {
  uint16 marker = 0xFFC5,
  uint16 marker_length,
  uint8 arithmetic_coding_conditioning,
}

seq DefineProgressiveHuffmanTable {
  uint16 marker = 0xFFC6,
  uint16 marker_length,
  uint8 progressive_huffman_table_id,
  uint8 class,
  uint8 num_codes,
  code_lengths: array[uint8],
  huffman_codes: array[uint8],
}

seq DefineLosslessHuffmanTable {
  uint16 marker = 0xFFC7,
  uint16 marker_length,
  uint8 lossless_huffman_table_id,
  uint8 class,
  uint8 num_codes,
  code_lengths: array[uint8],
  huffman_codes: array[uint8],
}

seq StartOfScan {
  uint16 marker = 0xFFDA,
  uint16 marker_length,
  uint8 num_components,
  components: array[ScanComponent],
  uint8 spectral_selection,
  uint8 successive_approximation,
  entropy_coded_data: array[uint8],
}

seq ScanComponent {
  uint8 id,
  uint8 dc_huffman_table_id,
  uint8 ac_huffman_table_id,
}

seq EndOfImage {
  uint16 marker = 0xFFD9,
  uint16 marker_length = 2,
}

seq RestartInterval {
  uint16 marker = 0xFFDD,
  uint16 marker_length,
  uint16 restart_interval,
}

seq ApplicationSpecific {
  uint16 marker = 0xFFE0-0xFFEF,
  uint16 marker_length,
  application_data: array[uint8],
}