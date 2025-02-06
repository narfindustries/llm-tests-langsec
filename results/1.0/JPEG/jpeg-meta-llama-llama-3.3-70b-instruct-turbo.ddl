seq jpeg_file = 
  magic: 0xFFD8,
  seq markers = 
    while not is_eoi: 
      marker: uint16(read(2)),
      switch marker: 
        case 0xFFE0: 
          read app0,
        case 0xFFE1: 
          read app1,
        case 0xFFE2: 
          read app2,
        case 0xFFE3: 
          read app3,
        case 0xFFE4: 
          read app4,
        case 0xFFE5: 
          read app5,
        case 0xFFE6: 
          read app6,
        case 0xFFE7: 
          read app7,
        case 0xFFE8: 
          read app8,
        case 0xFFE9: 
          read app9,
        case 0xFFEA: 
          read app10,
        case 0xFFEB: 
          read app11,
        case 0xFFEC: 
          read app12,
        case 0xFFED: 
          read app13,
        case 0xFFFE: 
          read comment,
        case 0xFFDB: 
          read dqt,
        case 0xFFC4: 
          read dht,
        case 0xFFC0: 
          read sof0,
        case 0xFFC2: 
          read sof1,
        case 0xFFC6: 
          read sof2,
        case 0xFFC3: 
          read sof3,
        case 0xFFC5: 
          read sof5,
        case 0xFFC7: 
          read sof6,
        case 0xFFC9: 
          read sof7,
        case 0xFFCA: 
          read sof9,
        case 0xFFCC: 
          read sof10,
        case 0xFFCD: 
          read sof11,
        case 0xFFDD: 
          read dri,
        case 0xFFDA: 
          read sos,
        case 0xFFD9: 
          break,

struct app0 = 
  length: uint16,
  identifier: string(5) = "JFIF\0",
  version: uint16,
  units: uint8,
  x_density: uint16,
  y_density: uint16,
  thumbnail_width: uint8,
  thumbnail_height: uint8,

struct app1 = 
  length: uint16,
  identifier: string(6) = "Exif\0\0",
  tiff_header: bytes(8) = "\x49\x49\x2a\x00\x08\x00\x00\x00",
  directory_offset: uint32,

struct app2 = 
  length: uint16,

struct app3 = 
  length: uint16,

struct app4 = 
  length: uint16,

struct app5 = 
  length: uint16,

struct app6 = 
  length: uint16,

struct app7 = 
  length: uint16,

struct app8 = 
  length: uint16,

struct app9 = 
  length: uint16,

struct app10 = 
  length: uint16,

struct app11 = 
  length: uint16,

struct app12 = 
  length: uint16,

struct app13 = 
  length: uint16,

struct comment = 
  length: uint16,
  comment: bytes(length - 2),

struct dqt = 
  length: uint16,
  quantization_table_number: uint8,
  quantization_table: bytes(64),

struct dht = 
  length: uint16,
  huffman_table_class: uint8,
  huffman_table_identifier: uint8,
  huffman_table: bytes(19),

struct sof0 = 
  length: uint16,
  precision: uint8,
  image_height: uint16,
  image_width: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sof0_component,

struct sof0_component = 
  component_id: uint8,
  horizontal_sampling_factor: uint8,
  vertical_sampling_factor: uint8,
  quantization_table_number: uint8,

struct sof1 = 
  length: uint16,
  precision: uint8,
  image_height: uint16,
  image_width: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sof1_component,

struct sof1_component = 
  component_id: uint8,
  horizontal_sampling_factor: uint8,
  vertical_sampling_factor: uint8,
  quantization_table_number: uint8,

struct sof2 = 
  length: uint16,
  precision: uint8,
  image_height: uint16,
  image_width: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sof2_component,

struct sof2_component = 
  component_id: uint8,
  horizontal_sampling_factor: uint8,
  vertical_sampling_factor: uint8,
  quantization_table_number: uint8,

struct sof3 = 
  length: uint16,
  precision: uint8,
  image_height: uint16,
  image_width: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sof3_component,

struct sof3_component = 
  component_id: uint8,
  horizontal_sampling_factor: uint8,
  vertical_sampling_factor: uint8,
  quantization_table_number: uint8,

struct sof5 = 
  length: uint16,
  precision: uint8,
  image_height: uint16,
  image_width: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sof5_component,

struct sof5_component = 
  component_id: uint8,
  horizontal_sampling_factor: uint8,
  vertical_sampling_factor: uint8,
  quantization_table_number: uint8,

struct sof6 = 
  length: uint16,
  precision: uint8,
  image_height: uint16,
  image_width: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sof6_component,

struct sof6_component = 
  component_id: uint8,
  horizontal_sampling_factor: uint8,
  vertical_sampling_factor: uint8,
  quantization_table_number: uint8,

struct sof7 = 
  length: uint16,
  precision: uint8,
  image_height: uint16,
  image_width: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sof7_component,

struct sof7_component = 
  component_id: uint8,
  horizontal_sampling_factor: uint8,
  vertical_sampling_factor: uint8,
  quantization_table_number: uint8,

struct sof9 = 
  length: uint16,
  precision: uint8,
  image_height: uint16,
  image_width: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sof9_component,

struct sof9_component = 
  component_id: uint8,
  horizontal_sampling_factor: uint8,
  vertical_sampling_factor: uint8,
  quantization_table_number: uint8,

struct sof10 = 
  length: uint16,
  precision: uint8,
  image_height: uint16,
  image_width: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sof10_component,

struct sof10_component = 
  component_id: uint8,
  horizontal_sampling_factor: uint8,
  vertical_sampling_factor: uint8,
  quantization_table_number: uint8,

struct sof11 = 
  length: uint16,
  precision: uint8,
  image_height: uint16,
  image_width: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sof11_component,

struct sof11_component = 
  component_id: uint8,
  horizontal_sampling_factor: uint8,
  vertical_sampling_factor: uint8,
  quantization_table_number: uint8,

struct dri = 
  length: uint16,
  restart_interval: uint16,

struct sos = 
  length: uint16,
  number_of_components: uint8,
  components: array(number_of_components) of sos_component,

struct sos_component = 
  component_id: uint8,
  dc_huffman_table_identifier: uint8,
  ac_huffman_table_identifier: uint8,