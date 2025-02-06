jpeg {
  soi: 0xFFD8;
  segments: segment*;
  eoi: 0xFFD9;
}

segment {
  marker: marker;
  length: uint16 if marker != soi && marker != eoi && marker != rstn;
  data: data[length - 2] if marker != soi && marker != eoi && marker != rstn;
}

marker {
  value: uint8;
  value: uint8 if value == 0xFF;
}

data {
  sof0: sof0_data if marker == 0xFFC0;
  sof1: sof1_data if marker == 0xFFC1;
  sof2: sof2_data if marker == 0xFFC2;
  sof3: sof3_data if marker == 0xFFC3;
  dht: dht_data if marker == 0xFFC4;
  dqt: dqt_data if marker == 0xFFDB;
  sos: sos_data if marker == 0xFFDA;
  appn: appn_data if marker >= 0xFFE0 && marker <= 0xFFEF;
  com: com_data if marker == 0xFFFE;
  dri: dri_data if marker == 0xFFDD;
  rstn: rstn_data if marker >= 0xFFD0 && marker <= 0xFFD7;
}

sof0_data {
  precision: uint8;
  height: uint16;
  width: uint16;
  num_components: uint8;
  components: component[num_components];
}

sof1_data {
  precision: uint8;
  height: uint16;
  width: uint16;
  num_components: uint8;
  components: component[num_components];
}

sof2_data {
  precision: uint8;
  height: uint16;
  width: uint16;
  num_components: uint8;
  components: component[num_components];
}

sof3_data {
  precision: uint8;
  height: uint16;
  width: uint16;
  num_components: uint8;
  components: component[num_components];
}

component {
  id: uint8;
  sampling_factors: uint8;
  quantization_table: uint8;
}

dht_data {
  table_class: uint8;
  destination: uint8;
  num_codes: uint8[16];
  symbols: uint8*;
}

dqt_data {
  precision: uint8;
  destination: uint8;
  table: uint8[64];
}

sos_data {
  num_components: uint8;
  components: sos_component[num_components];
  spectral_selection_start: uint8;
  spectral_selection_end: uint8;
  successive_approximation: uint8;
}

sos_component {
  id: uint8;
  dc_table: uint8;
  ac_table: uint8;
}

appn_data {
  identifier: uint8*;
  data: uint8*;
}

com_data {
  comment: uint8*;
}

dri_data {
  restart_interval: uint16;
}

rstn_data {
  // No additional data, just the marker
}