def Main = {
  header;
  blocks*;
  trailer
}

def header = {
  $"GIF"; 
  version : { $"87a" | $"89a" };
  screen_width  : uint16LE;
  screen_height : uint16LE;
  packed : uint8;
  bg_color_index : uint8;
  pixel_aspect_ratio : uint8
}

def blocks = {
  extension_block | image_descriptor
}

def extension_block = {
  $0x21;
  label : uint8;
  Match label {
    0xF9 => graphic_control_extension,
    0xFE => comment_extension,
    0x01 => plain_text_extension,
    0xFF => application_extension
  }
}

def graphic_control_extension = {
  block_size : uint8;
  packed : uint8;
  delay_time : uint16LE;
  transparent_color_index : uint8;
  terminator : uint8
}

def comment_extension = {
  data_blocks;
  $0x00
}

def plain_text_extension = {
  block_size : uint8;
  text_grid_left : uint16LE;
  text_grid_top : uint16LE;
  text_grid_width : uint16LE;
  text_grid_height : uint16LE;
  cell_width : uint8;
  cell_height : uint8;
  fg_color : uint8;
  bg_color : uint8;
  data_blocks;
  $0x00
}

def application_extension = {
  block_size : uint8;
  application_identifier : uint8[8];
  application_auth_code : uint8[3];
  data_blocks;
  $0x00
}

def data_blocks = {
  block_size : uint8;
  Match block_size {
    0 => @empty,
    _ => { data : uint8[block_size]; data_blocks }
  }
}

def image_descriptor = {
  $0x2C;
  left : uint16LE;
  top : uint16LE;
  width : uint16LE;
  height : uint16LE;
  packed : uint8;
  Match (packed & 0x80) {
    0 => @empty,
    _ => local_color_table
  };
  lzw_minimum_code_size : uint8;
  image_data : data_blocks
}

def local_color_table = {
  size : uint8[3 * (1 << ((packed & 0x07) + 1))]
}

def trailer = {
  $0x3B
}