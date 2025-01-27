def Main = {
  BitOrder BE;
  ByteOrder BE;
  nitf_file
}

def nitf_file = {
  file_header;
  @fill_bytes[8]
}

def file_header = {
  FH {
    magic = "NITF0";
    version = "2.1";
    complexity = uint8;
    standard_type = Choose {
      BCS_N {
        value = /[0-9A-Z]{4}/
      };
      BCS_A {
        value = /[A-Z]{4}/
      }
    };
    @padding[2];
    security_level = uint8;
    class_level = /[0-9A-F]{2}/;
    group_id = $(/[A-Z0-9]{6}/)
  }
}

def padding = {
  pad : uint8
}

def fill_bytes = {
  fill : uint8[]
}