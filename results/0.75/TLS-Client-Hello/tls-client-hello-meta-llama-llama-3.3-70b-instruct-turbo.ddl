domain tls {
  include "internet";
  include "transport";

  type TLSClientHello = struct {
    uint16 protocol_version,
    uint8 record_version,
    uint16 length,
    byte[32] session_id,
    byte[2] cipher_suite_length,
    uint16[1] cipher_suites,
    byte[1] compression_methods_length,
    byte[1] compression_method,
    byte[1] extensions_length,
    Extensions extensions
  };

  type Extensions = struct {
    uint16 extension_type,
    uint16 extension_length,
    byte[extension_length] extension_data
  };

  grammar TLSClientHello_grammar {
    TLSClientHello: 
      protocol_version: uint16,
      record_version: uint8,
      length: uint16,
      session_id: byte[32],
      cipher_suite_length: byte[2],
      cipher_suites: uint16[cipher_suite_length],
      compression_methods_length: byte[1],
      compression_method: byte[1],
      extensions_length: byte[2],
      extensions: Extensions[extensions_length]
  };

  language TLSClientHello_language {
    entry_point: TLSClientHello_grammar.TLSClientHello;
  };
};