format nitf {
  header: header_segment;
  image: image_segment?;
  graphic: graphic_segment?;
  text: text_segment?;
  data_extension: data_extension_segment?;
}

format header_segment {
  file_header: string(4) = "NITF";
  file_format_version: string(5) = "02.00";
  system_type: string(25);
  file_security_classification: classification;
  file_control_number: string(25);
  file_date_and_time: datetime;
  file_title: string(80);
  file_security_information: string(80);
  file_copy_number: uint32;
  file_number_of_copies: uint32;
  encrypted_file_flag: bool;
  encrypted_file_format: string(25);
  decryption_key: string(80);
  file_type: string(3);
  file_extension_flag: bool;
  file_extension_size: uint32;
  file_extension_data: bytes(file_extension_size);
}

format classification {
  value: string;
  U: classification = "U";
  C: classification = "C";
  S: classification = "S";
  T: classification = "T";
}

format compression_type {
  value: string;
  NC: compression_type = "NC";
  JB: compression_type = "JB";
  BC: compression_type = "BC";
}

format pixel_type {
  value: string;
  MON: pixel_type = "MON";
  RGB: pixel_type = "RGB";
}

format graphic_type {
  value: string;
  SIG: graphic_type = "SIG";
  SYM: graphic_type = "SYM";
}

format datetime {
  year: uint16;
  month: uint8;
  day: uint8;
  hour: uint8;
  minute: uint8;
  second: uint8;
}

format dimensions {
  width: uint32;
  height: uint32;
}

format image_segment {
  image_identifier: string(25);
  image_security_classification: classification;
  image_compression_type: compression_type;
  image_pixel_type: pixel_type;
  image_pixel_size: uint32;
  image_dimensions: dimensions;
  image_offset: uint32;
  image_data_length: uint32;
  image_data: bytes(image_data_length);
}

format graphic_segment {
  graphic_identifier: string(25);
  graphic_security_classification: classification;
  graphic_type: graphic_type;
  graphic_data_length: uint32;
  graphic_data: bytes(graphic_data_length);
}

format text_segment {
  text_identifier: string(25);
  text_security_classification: classification;
  text_data_length: uint32;
  text_data: bytes(text_data_length);
}

format data_extension_segment {
  data_extension_identifier: string(25);
  data_extension_security_classification: classification;
  data_extension_data_length: uint32;
  data_extension_data: bytes(data_extension_data_length);
}