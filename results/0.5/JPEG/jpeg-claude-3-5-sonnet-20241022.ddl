def Main = {
  JPEG_file
}

def JPEG_file = {
  markers_until_SOS;
  entropy_coded_data;
  EOI_marker
}

def markers_until_SOS = {
  SOI_marker;
  repeat1 {
    choice {
      APP0_marker;
      APP1_marker;
      DQT_marker;
      SOF0_marker;
      DHT_marker;
      SOS_marker
    }
  }
}

def SOI_marker = {
  Match (0xFF, 0xD8)
}

def APP0_marker = {
  Match (0xFF, 0xE0);
  length = UBE 2;
  identifier = Array length - 2 UInt8
}

def APP1_marker = {
  Match (0xFF, 0xE1);
  length = UBE 2;
  identifier = Array length - 2 UInt8
}

def DQT_marker = {
  Match (0xFF, 0xDB);
  length = UBE 2;
  table_data = Array length - 2 UInt8
}

def SOF0_marker = {
  Match (0xFF, 0xC0);
  length = UBE 2;
  precision = UInt8;
  height = UBE 2;
  width = UBE 2;
  components = Array 3 {
    component_id = UInt8;
    sampling_factors = UInt8;
    qt_number = UInt8
  }
}

def DHT_marker = {
  Match (0xFF, 0xC4);
  length = UBE 2;
  table_data = Array length - 2 UInt8
}

def SOS_marker = {
  Match (0xFF, 0xDA);
  length = UBE 2;
  ncomponents = UInt8;
  components = Array ncomponents {
    component_id = UInt8;
    huffman_table = UInt8
  };
  ignored = Array 3 UInt8
}

def entropy_coded_data = {
  repeat {
    choice {
      Match 0x00;
      Match !0xFF;
      ff_byte
    }
  }
}

def ff_byte = {
  Match 0xFF;
  Match 0x00
}

def EOI_marker = {
  Match (0xFF, 0xD9)
}