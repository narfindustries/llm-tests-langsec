def Main = {
  header;
  blocks*;
  trailer
}

def header = {
  $"GIF" $"89a";
  screen_descriptor;
  has_color_table = FBit;
  color_resolution = UInt 3;
  sort_flag = FBit;
  size_of_gct = UInt 3;
  background_color_index = UInt 8;
  pixel_aspect_ratio = UInt 8;
  if (has_color_table) { color_table[2 ^ (size_of_gct + 1)] }
}

def color_table[size] = {
  colors[size]
}

def colors[n] = {
  @n { color }
}

def color = {
  red = UInt 8;
  green = UInt 8;
  blue = UInt 8
}

def screen_descriptor = {
  width = UInt 16;
  height = UInt 16
}

def blocks = {
  | extension_block
  | image_block
}

def extension_block = {
  $0x21;
  | graphics_control_extension
  | comment_extension
  | application_extension
}

def graphics_control_extension = {
  $0xF9;
  block_size = UInt 8;
  flags = UInt 8;
  delay_time = UInt 16;
  transparent_color_index = UInt 8;
  terminator = UInt 8
}

def comment_extension = {
  $0xFE;
  sub_blocks
}

def application_extension = {
  $0xFF;
  block_size = UInt 8;
  application_id = Array 8 UInt 8;
  auth_code = Array 3 UInt 8;
  sub_blocks
}

def image_block = {
  $0x2C;
  left = UInt 16;
  top = UInt 16;
  width = UInt 16;
  height = UInt 16;
  has_lct = FBit;
  interlace = FBit;
  sort = FBit;
  reserved = UInt 2;
  lct_size = UInt 3;
  if (has_lct) { color_table[2 ^ (lct_size + 1)] };
  lzw_min_code_size = UInt 8;
  sub_blocks
}

def sub_blocks = {
  blocks_loop;
  $0x00
}

def blocks_loop = {
  block_size = UInt 8;
  if (block_size > 0) {
    block_data = Array block_size UInt 8;
    blocks_loop
  }
}

def trailer = {
  $0x3B
}