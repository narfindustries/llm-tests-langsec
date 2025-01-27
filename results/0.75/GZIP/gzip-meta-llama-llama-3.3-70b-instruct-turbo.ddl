domain gzip_meta {
  int32 magic;
  int8  compression_method;
  int8  flags;
  int32 modification_time;
  int32 extra_flags;
  int32 operating_system;
  sequence<header> header {
    int16 name_length;
    byte[name_length] name;
    int16 extra_length;
    byte[extra_length] extra;
  }
  byte[] compressed_data;
}

domain header {
  int16 name_length;
  byte[name_length] name;
  int16 extra_length;
  byte[extra_length] extra;
}