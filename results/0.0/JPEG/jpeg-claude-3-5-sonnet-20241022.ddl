def Main = {
  JPEG_file
}

def JPEG_file = {
  SOI;
  segments*;
  EOI
}

def segments = {
  segment_header;
  segment_data
}

def segment_header = {
  marker : uint 8;
  length : uint 16
}

def segment_data = {
  data : uint 8[length - 2]
}

def SOI = {
  marker : uint 16 = 0xFFD8
}

def EOI = {
  marker : uint 16 = 0xFFD9
}

def APP0 = {
  marker : uint 16 = 0xFFE0;
  length : uint 16;
  identifier : uint 8[5] = [0x4A, 0x46, 0x49, 0x46, 0x00];
  version : uint 16;
  units : uint 8;
  x_density : uint 16;
  y_density : uint 16;
  x_thumbnail : uint 8;
  y_thumbnail : uint 8;
  thumbnail_data : uint 8[x_thumbnail * y_thumbnail * 3]
}

def DQT = {
  marker : uint 16 = 0xFFDB;
  length : uint 16;
  qtable_data : uint 8[length - 2]
}

def SOF0 = {
  marker : uint 16 = 0xFFC0;
  length : uint 16;
  precision : uint 8;
  height : uint 16;
  width : uint 16;
  ncomponents : uint 8;
  components : component[ncomponents]
}

def component = {
  id : uint 8;
  sampling : uint 8;
  qtable_id : uint 8
}

def DHT = {
  marker : uint 16 = 0xFFC4;
  length : uint 16;
  htable_data : uint 8[length - 2]
}

def SOS = {
  marker : uint 16 = 0xFFDA;
  length : uint 16;
  ncomponents : uint 8;
  components : scan_component[ncomponents];
  spectral_selection_start : uint 8;
  spectral_selection_end : uint 8;
  successive_approximation : uint 8;
  entropy_coded_data : uint 8[]
}

def scan_component = {
  id : uint 8;
  dc_ac_table_ids : uint 8
}