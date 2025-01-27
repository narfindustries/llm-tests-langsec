domain NITF {
  types {
    uint8  = byte;
    uint16 = byte[2];
    uint32 = byte[4];
    uint64 = byte[8];
    int8   = byte;
    int16  = byte[2];
    int32  = byte[4];
    int64  = byte[8];
    string = byte[*];

    NITFHeader = struct {
      file_part_type: string[25];
      file_header_length: uint16;
      file_title: string[65];
      file_class: string[1];
      file_security_classification: string[1];
      file_control_number: string[20];
      file_date: string[8];
    };

    NITFMeta = struct {
      nitf_header: NITFHeader;
      file_header_extensions: byte[*];
      image_segment_header: byte[700];
      image_segment_data: byte[*];
    };
  }

  schema NITFMeta {
    nitf_header: NITFHeader;
    file_header_extensions: byte[*];
    image_segment_header: byte[700];
    image_segment_data: byte[*];
  }
}