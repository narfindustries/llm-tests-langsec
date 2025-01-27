def Main = {
  JPEG_stream
}

def JPEG_stream = {
  SOI marker_list EOI
}

def marker_list = {
  many marker
}

def marker = {
  $FF marker_type
}

def marker_type = {
  SOF0  |
  SOF2  |
  DHT   |
  DQT   |
  DRI   |
  SOS   |
  COM   |
  APP0  |
  APP1  |
  APP2  |
  APP3  |
  APP4  |
  APP5  |
  APP6  |
  APP7  |
  APP8  |
  APP9  |
  APP10 |
  APP11 |
  APP12 |
  APP13 |
  APP14 |
  APP15
}

def SOI = { $FF $D8 }
def EOI = { $FF $D9 }
def SOF0 = { $C0 length SOF_segment }
def SOF2 = { $C2 length SOF_segment }
def DHT = { $C4 length DHT_segment }
def DQT = { $DB length DQT_segment }
def DRI = { $DD length DRI_segment }
def SOS = { $DA length SOS_segment }
def COM = { $FE length COM_segment }
def APP0 = { $E0 length APP0_segment }
def APP1 = { $E1 length APP_segment }
def APP2 = { $E2 length APP_segment }
def APP3 = { $E3 length APP_segment }
def APP4 = { $E4 length APP_segment }
def APP5 = { $E5 length APP_segment }
def APP6 = { $E6 length APP_segment }
def APP7 = { $E7 length APP_segment }
def APP8 = { $E8 length APP_segment }
def APP9 = { $E9 length APP_segment }
def APP10 = { $EA length APP_segment }
def APP11 = { $EB length APP_segment }
def APP12 = { $EC length APP_segment }
def APP13 = { $ED length APP_segment }
def APP14 = { $EE length APP_segment }
def APP15 = { $EF length APP_segment }

def length = {
  len = uint16BE;
  SetLength (len - 2)
}

def SOF_segment = {
  precision : uint8;
  height : uint16BE;
  width : uint16BE;
  ncomponents : uint8;
  component_specs : arrayN ncomponents component_spec
}

def component_spec = {
  id : uint8;
  sampling_factors : uint8;
  qtable_id : uint8
}

def DHT_segment = {
  many1 huffman_table
}

def huffman_table = {
  table_info : uint8;
  codelengths : arrayN 16 uint8;
  values : array uint8
}

def DQT_segment = {
  many1 quantization_table
}

def quantization_table = {
  table_info : uint8;
  values : arrayN 64 uint8
}

def DRI_segment = {
  restart_interval : uint16BE
}

def SOS_segment = {
  ncomponents : uint8;
  component_specs : arrayN ncomponents scan_component_spec;
  spectral_selection_start : uint8;
  spectral_selection_end : uint8;
  approx_bit_pos : uint8;
  entropy_coded_data : array uint8
}

def scan_component_spec = {
  id : uint8;
  table_ids : uint8
}

def COM_segment = {
  comment : array uint8
}

def APP0_segment = {
  identifier : arrayN 5 uint8;
  version : uint16BE;
  units : uint8;
  xdensity : uint16BE;
  ydensity : uint16BE;
  xthumbnail : uint8;
  ythumbnail : uint8;
  thumbnail_data : array uint8
}

def APP_segment = {
  data : array uint8
}