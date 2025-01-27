def Main = {
  Jpeg;
}

def Marker = {
  marker_start : 0xFF;
  marker_type  : uint 8;
}

def SOI = {
  marker : Marker where marker.marker_type == 0xD8;
}

def EOI = {
  marker : Marker where marker.marker_type == 0xD9;
}

def APP0 = {
  marker      : Marker where marker.marker_type == 0xE0;
  length      : uint 16 BE;
  identifier  : Array 5 uint 8 where FD.contents == ['J','F','I','F',0x00'];
  version     : Array 2 uint 8;
  units       : uint 8;
  x_density   : uint 16 BE;
  y_density   : uint 16 BE;
  thumb_x     : uint 8;
  thumb_y     : uint 8;
  thumb_data  : Array (thumb_x * thumb_y * 3) uint 8;
}

def Frame = {
  marker          : Marker where marker.marker_type == 0xC0;
  length          : uint 16 BE;
  precision       : uint 8;
  height          : uint 16 BE;
  width           : uint 16 BE;
  num_components  : uint 8;
  components      : Array num_components {
    id            : uint 8;
    sampling      : uint 8;
    qt_id         : uint 8;
  };
}

def QuantTable = {
  marker    : Marker where marker.marker_type == 0xDB;
  length    : uint 16 BE;
  precision : uint 4;
  table_id  : uint 4;
  table     : Array 64 uint 8;
}

def Scan = {
  marker          : Marker where marker.marker_type == 0xDA;
  length          : uint 16 BE;
  num_components  : uint 8;
  components      : Array num_components {
    id            : uint 8;
    table_ids     : uint 8;
  };
  start_spec      : uint 8;
  end_spec        : uint 8;
  approx          : uint 8;
}

def DHTTable = {
  marker      : Marker where marker.marker_type == 0xC4;
  length      : uint 16 BE;
  table_class : uint 4;
  table_id    : uint 4;
  counts      : Array 16 uint 8;
  symbols     : Array (sum counts) uint 8;
}

def Jpeg = {
  soi           : SOI;
  app0          : APP0;
  quant_tables  : Many QuantTable;
  dht_tables    : Many DHTTable;
  frame         : Frame;
  scan          : Scan;
  image_data    : BitStream;
  eoi           : EOI;
}

def BitStream = {
  bytes : Many uint 8;
}