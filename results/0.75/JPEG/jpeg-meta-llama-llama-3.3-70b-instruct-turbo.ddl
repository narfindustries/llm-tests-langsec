domain JPEG {
  type bytes = byte[1..];
  type byte = int[0..255];
  type uint16 = int[0..65535];

  structure SOI {
    marker: byte = 0xFF;
    marker_type: byte = 0xD8;
  }

  structure APP0 {
    marker: byte = 0xFF;
    marker_type: byte = 0xE0;
    length: uint16;
    identifier: bytes = "JFIF\0";
    version_major: byte;
    version_minor: byte;
    units: byte;
    x_density: uint16;
    y_density: uint16;
    thumb_width: byte;
    thumb_height: byte;
  }

  structure DQT {
    marker: byte = 0xFF;
    marker_type: byte = 0xDB;
    length: uint16;
    table_info: byte;
    table_data: bytes;
  }

  structure SOF0 {
    marker: byte = 0xFF;
    marker_type: byte = 0xC0;
    length: uint16;
    precision: byte;
    height: uint16;
    width: uint16;
    num_components: byte;
    component_id: byte;
    horizontal_sampling_factor: byte;
    vertical_sampling_factor: byte;
    quantization_table_selector: byte;
  }

  structure DHT {
    marker: byte = 0xFF;
    marker_type: byte = 0xC4;
    length: uint16;
    table_info: byte;
    table_data: bytes;
  }

  structure SOS {
    marker: byte = 0xFF;
    marker_type: byte = 0xDA;
    length: uint16;
    num_components: byte;
    component_id: byte;
    dc_table_selector: byte;
    ac_table_selector: byte;
  }

  structure EOI {
    marker: byte = 0xFF;
    marker_type: byte = 0xD9;
  }

  structure Scan {
    sos: SOS;
    data: bytes;
    eoi: EOI;
  }

  structure JPEG {
    soi: SOI;
    app0: APP0;
    dqt: DQT;
    sof0: SOF0;
    dht: DHT;
    scan: Scan;
  }
}