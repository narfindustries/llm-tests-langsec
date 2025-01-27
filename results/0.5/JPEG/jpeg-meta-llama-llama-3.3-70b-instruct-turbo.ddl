domain jpeg {
  type byte = uint8;
  type uint16 = uint16;
  type uint32 = uint32;

  structure SOI {
    field magic: byte[2];
    assert magic == [0xFF, 0xD8], "Invalid SOI marker";
  }

  structure APP0 {
    field marker: byte[2];
    assert marker == [0xFF, 0xE0], "Invalid APP0 marker";
    field length: uint16;
    field identifier: byte[5];
    assert identifier == [0x4A, 0x46, 0x49, 0x46, 0x00], "Invalid identifier";
    field version: byte[2];
    field units: byte;
    field x_density: uint16;
    field y_density: uint16;
    field thumbnail_width: byte;
    field thumbnail_height: byte;
  }

  structure DQT {
    field marker: byte[2];
    assert marker == [0xFF, 0xDB], "Invalid DQT marker";
    field length: uint16;
    field precision: byte;
    field table_id: byte;
    field table: byte[64];
  }

  structure SOF0 {
    field marker: byte[2];
    assert marker == [0xFF, 0xC0], "Invalid SOF0 marker";
    field length: uint16;
    field precision: byte;
    field height: uint16;
    field width: uint16;
    field num_components: byte;
    field component_id: byte;
    field sampling_factors: byte;
    field quantization_table_id: byte;
  }

  structure DHT {
    field marker: byte[2];
    assert marker == [0xFF, 0xC4], "Invalid DHT marker";
    field length: uint16;
    field table_class: byte;
    field table_id: byte;
    field table: byte[16];
  }

  structure SOS {
    field marker: byte[2];
    assert marker == [0xFF, 0xDA], "Invalid SOS marker";
    field length: uint16;
    field num_components: byte;
    field component_id: byte;
    field dc_table_id: byte;
    field ac_table_id: byte;
  }

  structure EOI {
    field magic: byte[2];
    assert magic == [0xFF, 0xD9], "Invalid EOI marker";
  }

  structure JPEG {
    field soi: SOI;
    field app0: APP0;
    field dqt: DQT;
    field sof0: SOF0;
    field dht: DHT;
    field sos: SOS;
    field eoi: EOI;
  }
}