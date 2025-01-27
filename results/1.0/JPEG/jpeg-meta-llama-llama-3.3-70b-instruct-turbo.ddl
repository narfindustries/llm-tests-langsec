format jpeg {
  magic: uint16 = 0xffd8;

  header: jpeg_header {
    process: process_jpeg;
  }

  footer: uint16 = 0xffd9;
}

type jpeg_header {
  SOI: uint16 = 0xffd8;
  APP0: uint16 = 0xffe0;
  length: uint16;
  identifier: string(5) = "JFIF\0";
  version: uint16;
  units: uint8;
  xdensity: uint16;
  ydensity: uint16;
  thumbx: uint8;
  thumby: uint8;
}

process process_jpeg {
  marker: uint16;

  while marker != 0xffd9 {
    if marker == 0xffc0 {
      SOS: uint16 = 0xffc0;
      length: uint16;
      n: uint8;
      components: array(uint8, n);
      for i in 0..n-1 {
        id: uint8 = components[i];
        hd: uint8;
        vd: uint8;
      }
      se: uint8;
      scan: scan_data {
        id: id;
      }
    } else if marker == 0xffc4 {
      DHT: uint16 = 0xffc4;
      length: uint16;
      n: uint8;
      tables: array(table, n);
    } else if marker == 0xffda {
      SOS: uint16 = 0xffda;
      length: uint16;
      n: uint8;
      components: array(uint8, n);
      for i in 0..n-1 {
        id: uint8 = components[i];
        hd: uint8;
        vd: uint8;
      }
      se: uint8;
      scan: scan_data {
        id: id;
      }
    } else {
      length: uint16;
      data: bytes(length - 2);
    }

    marker: uint16;
  }
}

type scan_data {
  id: uint8;

  if id == 1 {
    y: bytes;
  } else if id == 2 {
    cb: bytes;
    cr: bytes;
  } else if id == 3 {
    y: bytes;
    cb: bytes;
    cr: bytes;
  }
}

type table {
  id: uint8;
  length: uint16;
  bytes: bytes(length - 2);
}