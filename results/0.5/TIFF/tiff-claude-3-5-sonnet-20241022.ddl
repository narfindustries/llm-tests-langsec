def Main = {
  TIFF_format
}

def TIFF_format = {
  header = IFD_Header;
  entries = IFD_Entries;
  $$ = {header, entries}
}

def IFD_Header = {
  byte_order = Choose {
    MM = "MM" $$ = "big-endian";
    II = "II" $$ = "little-endian"
  };
  version = uint16;
  offset = uint32;
  $$ = {byte_order, version, offset}
}

def IFD_Entries = {
  count = uint16;
  entries = Array count IFD_Entry;
  next_ifd = uint32;
  $$ = {count, entries, next_ifd}
}

def IFD_Entry = {
  tag = uint16;
  type = uint16;
  count = uint32;
  value_offset = uint32;
  $$ = {tag, type, count, value_offset}
}

def uint16 = !uint16BE | !uint16LE

def uint32 = !uint32BE | !uint32LE

def uint16BE = !BE uint16

def uint16LE = !LE uint16

def uint32BE = !BE uint32

def uint32LE = !LE uint32