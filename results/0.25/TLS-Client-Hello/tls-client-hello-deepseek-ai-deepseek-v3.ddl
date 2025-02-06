ClientHello = {
  legacy_version: UInt16;
  random: Bytes(32);
  legacy_session_id: LengthPrefixedBytes(UInt8);
  cipher_suites: LengthPrefixedList(UInt16, UInt16);
  legacy_compression_methods: LengthPrefixedBytes(UInt8);
  extensions: LengthPrefixedList(UInt16, Extension);
};

Extension = {
  extension_type: UInt16;
  extension_data: LengthPrefixedBytes(UInt16);
};

LengthPrefixedBytes(LengthType) = {
  length: LengthType;
  data: Bytes(length);
};

LengthPrefixedList(LengthType, ElementType) = {
  length: LengthType;
  elements: Array(ElementType, length / sizeof(ElementType));
};