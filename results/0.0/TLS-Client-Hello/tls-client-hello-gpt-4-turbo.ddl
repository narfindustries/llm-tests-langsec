module TLS.ClientHello {
  import TLS.Common;

  type ClientHello = struct {
    legacy_version: uint16;
    random: bytes[32];
    session_id: SessionID;
    cipher_suites: CipherSuites;
    compression_methods: CompressionMethods;
    extensions: Extensions;
  }

  type SessionID = struct {
    length: uint8;
    session_id: bytes[length];
  }

  type CipherSuites = struct {
    length: uint16;
    cipher_suites: uint16[length / 2];
  }

  type CompressionMethods = struct {
    length: uint8;
    compression_methods: uint8[length];
  }

  type Extensions = struct {
    length: uint16;
    extensions: Extension[length] until pos == length;
  }

  type Extension = struct {
    extension_type: uint16;
    extension_data_length: uint16;
    extension_data: bytes[extension_data_length];
  }
}