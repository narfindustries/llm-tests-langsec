domain NITF {
  doc "NITF (National Image Transmission Format) metadata"

  type NITFHEADER {
    doc "NITF file header"
    uint8[4] file_id; // "NITF"
    uint8[2] version; // e.g., "02"
    uint8[6] control;
    uint8[9] file_date;
    uint8[3] file_title;
    uint8[2] file_security;
    uint8[1] file_marking;
    uint8[4] file_describe;
    uint8[5] file_class;
    uint8[2] file_releas;
    uint8[20] file_ctlh;
    uint8[25] file_headers;
  }

  type NITF_META {
    doc "NITF metadata"
    NITFHEADER header;
    uint8[2] meta_version; // e.g., "01"
    uint8[8] meta_data;
  }

  type NITF_FILE {
    doc "NITF file structure"
    NITFHEADER file_header;
    meta: NITF_META;
  }

  root NITF_FILE;
}