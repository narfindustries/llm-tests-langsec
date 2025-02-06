format jpeg {
  marker: 0xFF
  marker_type: uint8

  switch marker_type {
    case 0xD8: StartOfImage
    case 0xD9: EndOfImage
    case 0xDA: StartOfScan
    case 0xDB: DefineQuantizationTable
    case 0xDC: DefineHuffmanTable
    case 0xDD: DefineArithmeticCodingConditioning
    case 0xDE: DefineProgressiveHuffmanTable
    case 0xDF: DefineLosslessHuffmanTable
    case 0xE0: ApplicationSegment0
    case 0xE1: ApplicationSegment1
    case 0xE2: ApplicationSegment2
    case 0xE3: ApplicationSegment3
    case 0xE4: ApplicationSegment4
    case 0xE5: ApplicationSegment5
    case 0xE6: ApplicationSegment6
    case 0xE7: ApplicationSegment7
    case 0xE8: ApplicationSegment8
    case 0xE9: ApplicationSegment9
    case 0xEA: ApplicationSegment10
    case 0xEB: ApplicationSegment11
    case 0xEC: ApplicationSegment12
    case 0xED: ApplicationSegment13
    case 0xEE: ApplicationSegment14
    case 0xEF: ApplicationSegment15
  }
}

format StartOfImage {
  length: uint16
  precision: uint8
  image_height: uint16
  image_width: uint16
  num_components: uint8
  components: array[Component](num_components)
}

format Component {
  id: uint8
  horizontal_sampling_factor: uint8
  vertical_sampling_factor: uint8
  quantization_table_number: uint8
}

format EndOfImage {
}

format StartOfScan {
  length: uint16
  num_components: uint8
  components: array[ScanComponent](num_components)
  spectral_selection_start: uint8
  spectral_selection_end: uint8
  approximation_high: uint8
  approximation_low: uint8
}

format ScanComponent {
  id: uint8
  dc_huffman_table_number: uint8
  ac_huffman_table_number: uint8
}

format DefineQuantizationTable {
  length: uint16
  quantization_table_number: uint8
  quantization_table_precision: uint8
  quantization_table: array[uint8](64)
}

format DefineHuffmanTable {
  length: uint16
  huffman_table_class: uint8
  huffman_table_identifier: uint8
  number_of_codes: uint8
  code_lengths: array[uint8](number_of_codes)
  huffman_codes: array[uint8](number_of_codes)
}

format DefineArithmeticCodingConditioning {
  length: uint16
  arith_code_cond_spec: uint8
}

format DefineProgressiveHuffmanTable {
  length: uint16
  huffman_table_class: uint8
  huffman_table_identifier: uint8
  number_of_codes: uint8
  code_lengths: array[uint8](number_of_codes)
  huffman_codes: array[uint8](number_of_codes)
}

format DefineLosslessHuffmanTable {
  length: uint16
  num_components: uint8
  components: array[LosslessComponent](num_components)
}

format LosslessComponent {
  id: uint8
  huffman_table_number: uint8
}

format ApplicationSegment0 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment1 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment2 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment3 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment4 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment5 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment6 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment7 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment8 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment9 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment10 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment11 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment12 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment13 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment14 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}

format ApplicationSegment15 {
  length: uint16
  identifier: uint8
  data: array[uint8](length - 2)
}