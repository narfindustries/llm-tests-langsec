jpeg: 
  soi: uint16 = 0xFFD8
  app0: 
    marker: uint16 = 0FFE0
    length: uint16
    identifier: string(4) = "JFIF"
    version: uint8
    units: uint8
    x_density: uint16
    y_density: uint16
    thumb_width: uint8
    thumb_height: uint8
  app1: 
    marker: uint16 = 0FFE1
    length: uint16
    identifier: string
    data: bytes
  dqt: 
    marker: uint16 = 0FFDB
    length: uint16
    table_number: uint8
    precision: uint8
    quantization_table: array(64, uint8)
  dht: 
    marker: uint16 = 0FFC4
    length: uint16
    table_class: uint8
    table_number: uint8
    number_of_codes: uint16
    huffman_code: bytes
  sofo: 
    marker: uint16 = 0FFC0
    length: uint16
    precision: uint8
    image_height: uint16
    image_width: uint16
    number_of_components: uint8
    components: 
      identifier: uint8
      horizontal_sampling_factor: uint8
      vertical_sampling_factor: uint8
      quantization_table_number: uint8
  dri: 
    marker: uint16 = 0FFDD
    length: uint16
    restart_interval: uint16
  sos: 
    marker: uint16 = 0FFDA
    length: uint16
    number_of_components: uint8
    components: 
      identifier: uint8
      horizontal_sampling_factor: uint8
      vertical_sampling_factor: uint8
      quantization_table_number: uint8
    dc_entropy_coding: uint8
    ac_tables: uint8
  scan_data: bytes
  rst: 
    marker: uint16 = range(0xFFD0, 0xFFD7)
  eoi: uint16 = 0xFFD9