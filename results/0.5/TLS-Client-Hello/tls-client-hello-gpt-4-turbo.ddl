module TLS_ClientHello;

type uint8 = UInt<8>;
type uint16 = UInt<16>;
type uint32 = UInt<32>;

type Random = struct {
  gmt_unix_time : uint32;
  random_bytes : array[uint8, 28];
};

type CipherSuites = struct {
  length : uint16;
  suites : array[uint16, length / 2];
};

type CompressionMethods = struct {
  length : uint8;
  methods : array[uint8, length];
};

type Extension = struct {
  extension_type : uint16;
  extension_data_length : uint16;
  extension_data : array[uint8, extension_data_length];
};

type Extensions = struct {
  length : uint16;
  extension_list : array[Extension];
};

type ClientHello = struct {
  legacy_version : uint16 = 0x0303;
  random : Random;
  legacy_session_id : array[uint8];
  cipher_suites : CipherSuites;
  legacy_compression_methods : CompressionMethods;
  extensions : Extensions;
};