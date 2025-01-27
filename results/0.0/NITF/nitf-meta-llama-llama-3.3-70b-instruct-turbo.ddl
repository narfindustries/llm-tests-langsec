domain NITF {
  types {
    uint8  : 1 byte;
    uint16 : 2 bytes;
    uint32 : 4 bytes;
    uint64 : 8 bytes;
    string : sequence of byte;
  }

  type NITF_META_LLAMA {
    fields {
      FILE_TITLE: string(25);
      FILE_DATE: string(10);
      FILE_TIME: string(8);
      FILE_SIZE: uint32;
      FILE_HEADER_LENGTH: uint16;
    }
  }

  type NITF_HEADER {
    fields {
      NITF_META_LLAMA: NITF_META_LLAMA;
      RESERVED: uint8[2];
      FILE_HEADER_LENGTH: uint16;
    }
  }

  type NITF_FILE {
    fields {
      NITF_HEADER: NITF_HEADER;
      FILE_DATA: sequence of byte;
    }
  }

  grammar {
    NITF_FILE: NITF_HEADER FILE_DATA;
    NITF_HEADER: NITF_META_LLAMA RESERVED FILE_HEADER_LENGTH;
    NITF_META_LLAMA: FILE_TITLE FILE_DATE FILE_TIME FILE_SIZE FILE_HEADER_LENGTH;
  }

  start NITF_FILE;
}