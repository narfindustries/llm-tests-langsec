format tiff {
  byte_order: uint16 = 0x4949 | 0x4D4D;
  magic_number: uint16 = 0x002A;
  offset_to_first_ifd: uint32;

  ifd: array {
    count: uint16;
    entries: array {
      tag_number: uint16;
      tag_type: uint16;
      tag_count: uint32;
      tag_value_offset: uint32;
    };
  };

  ifd_entries: choice(
    image_width: {
      tag_number: 0x0100;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    image_length: {
      tag_number: 0x0101;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    bits_per_sample: {
      tag_number: 0x0102;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    compression: {
      tag_number: 0x0103;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    photometric_interpretation: {
      tag_number: 0x0106;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    orientation: {
      tag_number: 0x010E;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    samples_per_pixel: {
      tag_number: 0x0115;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    rows_per_strip: {
      tag_number: 0x0116;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    strip_offsets: {
      tag_number: 0x0117;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    strip_byte_counts: {
      tag_number: 0x0118;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    min_sample_value: {
      tag_number: 0x0119;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    max_sample_value: {
      tag_number: 0x011A;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    x_resolution: {
      tag_number: 0x011A;
      tag_type: 0x0005;
      tag_count: 1;
      tag_value: rational;
    },
    y_resolution: {
      tag_number: 0x011B;
      tag_type: 0x0005;
      tag_count: 1;
      tag_value: rational;
    },
    planar_configuration: {
      tag_number: 0x011C;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    page_name: {
      tag_number: 0x011D;
      tag_type: 0x0002;
      tag_count: 1;
      tag_value: string;
    },
    x_position: {
      tag_number: 0x011E;
      tag_type: 0x0005;
      tag_count: 1;
      tag_value: rational;
    },
    y_position: {
      tag_number: 0x011F;
      tag_type: 0x0005;
      tag_count: 1;
      tag_value: rational;
    },
    free_offsets: {
      tag_number: 0x0120;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    free_byte_counts: {
      tag_number: 0x0121;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    gray_response_unit: {
      tag_number: 0x0122;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    gray_response_curve: {
      tag_number: 0x0123;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    t4_options: {
      tag_number: 0x0124;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    t6_options: {
      tag_number: 0x0125;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    resolution_unit: {
      tag_number: 0x0128;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    page_number: {
      tag_number: 0x0129;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    transfer_function: {
      tag_number: 0x012D;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    software: {
      tag_number: 0x0131;
      tag_type: 0x0002;
      tag_count: 1;
      tag_value: string;
    },
    datetime: {
      tag_number: 0x0132;
      tag_type: 0x0002;
      tag_count: 1;
      tag_value: string;
    },
    artist: {
      tag_number: 0x013B;
      tag_type: 0x0002;
      tag_count: 1;
      tag_value: string;
    },
    host_computer: {
      tag_number: 0x013C;
      tag_type: 0x0002;
      tag_count: 1;
      tag_value: string;
    },
    predictor: {
      tag_number: 0x013D;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    white_point: {
      tag_number: 0x013E;
      tag_type: 0x0005;
      tag_count: 1;
      tag_value: rational;
    },
    primary_chromaticities: {
      tag_number: 0x013F;
      tag_type: 0x0005;
      tag_count: 1;
      tag_value: rational;
    },
    color_map: {
      tag_number: 0x0140;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    halftone_hints: {
      tag_number: 0x0141;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    tile_width: {
      tag_number: 0x0142;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    tile_length: {
      tag_number: 0x0143;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    tile_offsets: {
      tag_number: 0x0144;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    tile_byte_counts: {
      tag_number: 0x0145;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    sub_ifds: {
      tag_number: 0x014A;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    ink_set: {
      tag_number: 0x014C;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    ink_names: {
      tag_number: 0x014D;
      tag_type: 0x0002;
      tag_count: 1;
      tag_value: string;
    },
    number_of_inks: {
      tag_number: 0x014E;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    dot_range: {
      tag_number: 0x0150;
      tag_type: 0x0001;
      tag_count: 1;
      tag_value: byte;
    },
    target_printer: {
      tag_number: 0x0151;
      tag_type: 0x0002;
      tag_count: 1;
      tag_value: string;
    },
    extra_samples: {
      tag_number: 0x0152;
      tag_type: 0x0001;
      tag_count: 1;
      tag_value: byte;
    },
    sample_format: {
      tag_number: 0x0153;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    s_min_sample_value: {
      tag_number: 0x0154;
      tag_type: 0x0001;
      tag_count: 1;
      tag_value: byte;
    },
    s_max_sample_value: {
      tag_number: 0x0155;
      tag_type: 0x0001;
      tag_count: 1;
      tag_value: byte;
    },
    transfer_range: {
      tag_number: 0x0156;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    clip_path: {
      tag_number: 0x0157;
      tag_type: 0x0001;
      tag_count: 1;
      tag_value: byte;
    },
    x_clip_path_units: {
      tag_number: 0x0158;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    y_clip_path_units: {
      tag_number: 0x0159;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    indexed: {
      tag_number: 0x015A;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    jpeg_tables: {
      tag_number: 0x015B;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    opi_proxy: {
      tag_number: 0x015C;
      tag_type: 0x0003;
      tag_count: 1;
      tag_value: uint16;
    },
    global_parameters_ifd: {
      tag_number: 0x0190;
      tag_type: 0x0004;
      tag_count: 1;
      tag_value: uint32;
    },
    profile_type: {
      tag_number: 0x0191;
      tag_type: 0x0001;
      tag_count: 1;
      tag_value: byte;
    },
    fax_profile: {
      tag_number: 0x0192;
      tag_type: 0x0001;
      tag_count: 1;
      tag_value: byte;
    },
    coding_methods: {
      tag_number: 0x0193;
      tag_type: 0x0001;
      tag_count: 1;
      tag_value: byte;
    },
    version_year: {
      tag_number: 0x0194;
      tag_type: 0x0001;
      tag_count: 1;
      tag_value: byte;
    },
    mode_number: {
      tag_number: 0x0195;
      tag_type: 0x0001;
      tag_count: 1;
      tag_value: byte;
    },
  );

  rational: {
    numerator: uint32;
    denominator: uint32;
  };
}