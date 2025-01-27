domain JPEG {
  type SOI = byte[2] aligned(1) {
    value == 0xffd8
  }

  type APP0 = struct {
    marker: byte[2] aligned(1) {
      value == 0xffe0
    }
    length: uint16be aligned(2)
    identifier: byte[5] aligned(1) {
      value == "JFIF\0"
    }
    version: byte[2] aligned(1)
    density_units: byte aligned(1)
    x_density: uint16be aligned(2)
    y_density: uint16be aligned(2)
    thumbnail_height: byte aligned(1)
    thumbnail_width: byte aligned(1)
  } aligned(2)

  type DQT = struct {
    marker: byte[2] aligned(1) {
      value == 0xffdb
    }
    length: uint16be aligned(2)
    precision: byte aligned(1)
    table_id: byte aligned(1)
    table: byte[64] aligned(1)
  } aligned(2)

  type DHT = struct {
    marker: byte[2] aligned(1) {
      value == 0xffc4
    }
    length: uint16be aligned(2)
    table_class: byte aligned(1)
    table_id: byte aligned(1)
    num_symbols: byte aligned(1)
    code_lengths: byte[16] aligned(1)
    huffman_codes: byte[num_symbols] aligned(1)
  } aligned(2)

  type SOS = struct {
    marker: byte[2] aligned(1) {
      value == 0xffda
    }
    length: uint16be aligned(2)
    num_components: byte aligned(1)
    component_ids: byte[num_components] aligned(1)
    huffman_tables: byte[num_components * 2] aligned(1)
  } aligned(2)

  type EOI = byte[2] aligned(1) {
    value == 0xffd9
  }

  type JPEG_file = struct {
    soi: SOI
    app0: APP0
    dqt: DQT
    dht: DHT
    sos: SOS
    eoi: EOI
  } aligned(1)
}