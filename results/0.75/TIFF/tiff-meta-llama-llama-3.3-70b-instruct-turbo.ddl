segment tiff {
  byte_order: uint16 = 0x4949,
  magic_number: uint16 = 0x002a,
  ifd_offset: uint32
}

segment ifd {
  count: uint16,
  entries: array[count] of ifd_entry,
  next_ifd_offset: uint32
}

segment ifd_entry {
  tag_number: uint16,
  tag_type: tag_type,
  tag_count: uint32,
  tag_value_offset: uint32
}

enum tag_type: uint16 {
  byte = 1,
  ascii = 2,
  short = 3,
  long = 4,
  rational = 5,
  sbyte = 6,
  undefined = 7,
  sshort = 8,
  slong = 9,
  srational = 10,
  float = 11,
  double = 12
}

enum tag_number: uint16 {
  image_width = 256,
  image_length = 257,
  bits_per_sample = 258,
  compression = 259,
  photometric_interpretation = 262,
  orientation = 274,
  samples_per_pixel = 277,
  rows_per_strip = 278,
  strip_offsets = 279,
  strip_byte_counts = 280,
  x_resolution = 282,
  y_resolution = 283,
  planar_configuration = 284,
  page_name = 285,
  x_position = 286,
  y_position = 287,
  free_offsets = 288,
  free_byte_counts = 289,
  gray_response_unit = 290,
  gray_response_curve = 291,
  t4_options = 292,
  t6_options = 293,
  resolution_unit = 296,
  page_number = 297,
  transfer_function = 301,
  software = 305,
  date_time = 306,
  artist = 315,
  host_computer = 316,
  predictor = 317,
  white_point = 318,
  primary_chromaticities = 319,
  color_map = 320,
  halftone_hints = 321,
  tile_width = 322,
  tile_length = 323,
  tile_offsets = 324,
  tile_byte_counts = 325,
  sub_ifds = 330,
  transfer_range = 342,
  jpeg_tables = 347,
  jpeg_proc = 348,
  jpeg_interchange_format = 349,
  jpeg_interchange_format_length = 350,
  ycbcr_coefficients = 529,
  ycbcr_sub_sampling = 530,
  ycbcr_positioning = 531,
  reference_black_white = 532
}

enum compression: uint16 {
  no_compression = 1,
  ccitt_group_3 = 2,
  ccitt_group_4 = 3,
  lzw = 4,
  packbits = 5,
  jpeg = 6
}

enum photometric_interpretation: uint16 {
  white_is_zero = 0,
  black_is_zero = 1,
  rgb = 2,
  palette_color = 3,
  transparency_mask = 4,
  cmyk = 5,
  ycbcr = 6
}

enum orientation: uint16 {
  top_left = 1,
  top_right = 2,
  bottom_right = 3,
  bottom_left = 4,
  left_top = 5,
  right_top = 6,
  right_bottom = 7,
  left_bottom = 8
}

enum planar_configuration: uint16 {
  chunky = 1,
  planar = 2
}

enum resolution_unit: uint16 {
  none = 1,
  inch = 2,
  centimeter = 3
}

enum predictor: uint16 {
  no_prediction = 1,
  horizontal = 2
}

enum ycbcr_sub_sampling: uint16 {
  subsample_1x1 = 1,
  subsample_2x2 = 2,
  subsample_4x4 = 3
}

enum ycbcr_positioning: uint16 {
  co_sited = 1,
  non_co_sited = 2
}