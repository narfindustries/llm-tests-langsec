domain TLS_Client_Hello {
  import "TLS handshake"

  type record TLS_Client_Hello {
    uint16 protocol_version
    uint8 record_length_msb
    uint8 record_length_lsb
    uint8 handshake_type
    uint8 handshake_length_msb
    uint8 handshake_length_lsb
    uint8 handshake_length_lsb_2
    uint16 version
    uint32 gmt_unix_time
    uint28 random_bytes
    uint8 session_id_length
    uint8 session_id
    uint16 cipher_suites_length
    uint16 cipher_suite
    uint8 compression_methods_length
    uint8 compression_method
    uint32 extensions_length
    uint16 extension_type
    uint16 extension_length
    uint16 ec_point_formats_length
    uint8 ec_point_format
    uint16 elliptic_curves_length
    uint16 elliptic_curve

    constraints {
      record_length_msb == (record_length_lsb << 8)
      handshake_length_msb == (handshake_length_lsb_2 << 8)
      version == 0x0303
      handshake_type == 0x01
      cipher_suites_length >= 2
      compression_methods_length >= 1
      extensions_length >= 2
    }
  }

  grammar TLS_Client_Hello_grammar {
    TLS_Client_Hello: 
      protocol_version: uint16 
      record_length_msb: uint8 
      record_length_lsb: uint8 
      handshake_type: uint8 
      handshake_length_msb: uint8 
      handshake_length_lsb: uint8 
      handshake_length_lsb_2: uint8 
      version: uint16 
      gmt_unix_time: uint32 
      random_bytes: uint28 
      session_id_length: uint8 
      session_id: uint8 
      cipher_suites_length: uint16 
      cipher_suite: uint16 
      compression_methods_length: uint8 
      compression_method: uint8 
      extensions_length: uint32 
      extension_type: uint16 
      extension_length: uint16 
      ec_point_formats_length: uint16 
      ec_point_format: uint8 
      elliptic_curves_length: uint16 
      elliptic_curve: uint16;
  }
}