def Main = {
  header;
  blocks*;
  $$ Commit
}

def header = {
  magic_bytes;
  screen_descriptor;
  has_gct;
  gct?;
  blocks*
}

def magic_bytes = {
  magic : !FD "GIF";
  version : !FD ("87a" | "89a")
}

def screen_descriptor = {
  width : uint16LE;
  height : uint16LE;
  packed : uint8;
  bg_color_index : uint8;
  pixel_aspect_ratio : uint8
}

def has_gct = packed >> 7 == 1

def gct = {
  size : uint32LE;
  gct_data : uint8[size]
}

def blocks = {
  block_type : uint8;
  match block_type {
    0x2C => image_block
    0x21 => extension_block
    0x3B => trailer
    _    => fail
  }
}

def image_block = {
  left : uint16LE;
  top : uint16LE;
  width : uint16LE;
  height : uint16LE;
  packed : uint8;
  lct?;
  image_data
}

def lct = has_lct {
  lct_data : uint8[lct_size]
}

def has_lct = @packed >> 7 == 1

def lct_size = 3 * (2 << (@packed & 0x7))

def extension_block = {
  label : uint8;
  match label {
    0xF9 => graphic_control_extension
    0xFE => comment_extension
    0x01 => plain_text_extension
    0xFF => application_extension
    _    => fail
  }
}

def graphic_control_extension = {
  size : uint8;
  block_data : uint8[size];
  terminator : uint8
}

def comment_extension = {
  data_blocks*;
  terminator : uint8
}

def plain_text_extension = {
  size : uint8;
  text_data : uint8[size];
  data_blocks*;
  terminator : uint8
}

def application_extension = {
  block_size : uint8;
  app_id : uint8[block_size];
  data_blocks*;
  terminator : uint8
}

def data_blocks = {
  size : uint8;
  data : uint8[size]
}

def image_data = {
  lzw_min_code_size : uint8;
  data_blocks*;
  terminator : uint8
}

def trailer = uint8 == 0x3B