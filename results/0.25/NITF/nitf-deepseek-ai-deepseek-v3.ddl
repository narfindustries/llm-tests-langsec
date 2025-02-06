nitf_file {
  file_header: file_header;
  image_segments: image_segment*;
  graphic_segments: graphic_segment*;
  text_segments: text_segment*;
}

file_header {
  FHDR: "NITF";
  FSIZE: uint64;
  CLEVEL: uint8;
  STYPE: string(4);
  OSTAID: string(10);
  FDT: string(14);
  FTITLE: string(80);
  FSCLAS: string(1);
  FSCODE: string(40);
  FSCTLH: string(40);
  FSREL: string(20);
  FSCAUT: string(20);
  FSCTLN: string(20);
  FSCOP: string(5);
  FSCPYS: string(5);
  ONAME: string(27);
  OPHONE: string(18);
}

image_segment {
  image_header: image_header;
  image_data: bytes;
}

image_header {
  IM: "IM";
  IID: string(10);
  IDATIM: string(14);
  TGTID: string(17);
  IID2: string(80);
  ISCLAS: string(1);
  ISCODE: string(40);
  ISCTLH: string(40);
  ISREL: string(20);
  ISCAUT: string(20);
  ISCTLN: string(20);
  ISCOP: string(5);
  ISCPYS: string(5);
  ENCRYP: uint8;
  ISORCE: string(42);
  NROWS: uint64;
  NCOLS: uint64;
  PVTYPE: string(3);
  IREP: string(8);
  ICAT: string(8);
  ABPP: uint16;
  PJUST: string(1);
  ICORDS: string(1);
}

graphic_segment {
  graphic_header: graphic_header;
  graphic_data: bytes;
}

graphic_header {
  GS: "GS";
  SID: string(10);
  SNAME: string(20);
  SCLAS: string(1);
  SCODE: string(40);
  SCTLH: string(40);
  SREL: string(20);
  SCAUT: string(20);
  SCTLN: string(20);
  SCOP: string(5);
  SCPYS: string(5);
}

text_segment {
  text_header: text_header;
  text_data: bytes;
}

text_header {
  TX: "TX";
  TEXTID: string(7);
  TXTALVL: uint8;
  TXTDT: string(14);
  TXTITL: string(80);
  TSCLAS: string(1);
  TSCODE: string(40);
  TSCTLH: string(40);
  TSREL: string(20);
  TSCAUT: string(20);
  TSCTLN: string(20);
}