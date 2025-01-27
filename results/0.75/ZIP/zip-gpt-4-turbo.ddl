-- Daedalus .ddl file for parsing a ZIP file format

type U4 = UInt 32

type U2 = UInt 16

type U1 = UInt 8

type EOCD = struct {
  disk_num           : U2;
  start_disk_num     : U2;
  num_records        : U2;
  total_records      : U2;
  central_dir_size   : U4;
  central_dir_offset : U4;
  comment_len        : U2;
  comment            : Bytes .Size(comment_len);
}

type CentralDirEntry = struct {
  version_made       : U2;
  version_needed     : U2;
  flag               : U2;
  compression        : U2;
  mod_time           : U2;
  mod_date           : U2;
  crc32              : U4;
  compressed_size    : U4;
  uncompressed_size  : U4;
  filename_len       : U2;
  extra_field_len    : U2;
  comment_len        : U2;
  disk_start         : U2;
  internal_attr      : U2;
  external_attr      : U4;
  local_header_offset: U4;
  filename           : Bytes .Size(filename_len);
  extra_field        : Bytes .Size(extra_field_len);
  comment            : Bytes .Size(comment_len);
}

type LocalFileHeader = struct {
  version_needed     : U2;
  flag               : U2;
  compression        : U2;
  mod_time           : U2;
  mod_date           : U2;
  crc32              : U4;
  compressed_size    : U4;
  uncompressed_size  : U4;
  filename_len       : U2;
  extra_field_len    : U2;
  filename           : Bytes .Size(filename_len);
  extra_field        : Bytes .Size(extra_field_len);
  data               : Bytes .Size(compressed_size);
}

type ZIPFile = struct {
  files              : [LocalFileHeader];
  _                  : Seek (getCentralDirOffset());
  central_dir        : [CentralDirEntry] .Until (@.offset == getEOCDOffset());
  eocd               : EOCD;
}

function getCentralDirOffset(): [U1] {
  Seek(-22, SeekEnd);
  FindEOCD();
  return u4;
}

function FindEOCD():[U1] {
  if Peek .Size(4) != [0x50, 0x4b, 0x05, 0x06] {
    Seek(-5, SeekCur);
    FindEOCD();
  }
}

function getEOCDOffset(): U4 {
  return current_offset - 22;
}
