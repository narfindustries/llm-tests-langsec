def Main = {
  header;
  frames
}

def header = {
  signature;
  screen_descriptor;
  has_gct = $$ uint 1;
  color_res = $$ uint 3;
  sort_flag = $$ uint 1;
  gct_size = $$ uint 3;
  Match has_gct {
    1 => global_color_table
  }
}

def signature = {
  $$ FByte ('G');
  $$ FByte ('I');
  $$ FByte ('F');
  $$ FByte ('8');
  $$ FByte ('9');
  $$ FByte ('a')
}

def screen_descriptor = {
  width = $$ uint 16;
  height = $$ uint 16;
  packed = $$ uint 8;
  bg_color_index = $$ uint 8;
  pixel_aspect_ratio = $$ uint 8
}

def global_color_table = {
  colors = @Array(3 * (2 ^ (gct_size + 1))) {
    r = $$ uint 8;
    g = $$ uint 8;
    b = $$ uint 8
  }
}

def frames = @Until (Choose {
  $$ uint 8 == 0x3B => trailer;
  $$ uint 8 == 0x21 => extension;
  $$ uint 8 == 0x2C => image_block
})

def trailer = $$ null

def extension = {
  intro = $$ uint 8;
  Match intro {
    0xF9 => graphic_control_ext;
    0xFE => comment_ext;
    0xFF => application_ext;
    0x01 => plain_text_ext
  }
}

def graphic_control_ext = {
  block_size = $$ uint 8;
  packed = $$ uint 8;
  delay_time = $$ uint 16;
  transparent_color_index = $$ uint 8;
  terminator = $$ uint 8
}

def comment_ext = data_blocks

def application_ext = {
  block_size = $$ uint 8;
  app_id = $$ Bytes 8;
  auth_code = $$ Bytes 3;
  data = data_blocks
}

def plain_text_ext = {
  block_size = $$ uint 8;
  text_grid_params = $$ Bytes 12;
  data = data_blocks
}

def image_block = {
  left = $$ uint 16;
  top = $$ uint 16;
  width = $$ uint 16;
  height = $$ uint 16;
  packed = $$ uint 8;
  has_lct = $$ uint 1;
  interlace = $$ uint 1;
  sort = $$ uint 1;
  reserved = $$ uint 2;
  lct_size = $$ uint 3;
  Match has_lct {
    1 => local_color_table
  };
  image_data
}

def local_color_table = {
  colors = @Array(3 * (2 ^ (lct_size + 1))) {
    r = $$ uint 8;
    g = $$ uint 8;
    b = $$ uint 8
  }
}

def image_data = {
  lzw_min_code_size = $$ uint 8;
  data = data_blocks
}

def data_blocks = @Until (
  $$ uint 8 == 0x00
) {
  block_size = $$ uint 8;
  data = $$ Bytes block_size
}