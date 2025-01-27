module TLS.ClientHello {
  import ByteOrder::BigEndian;
  import layout::bytes;
  import layout::word16;
  import layout::word24;
  import layout::word32;

  ++type ProtocolVersion = struct {
    major : byte;
    minor : byte;
  };

  ++type Random = struct {
    gmt_unix_time : word32;
    random_bytes : bytes[28];
  };

  ++type SessionID = struct {
    length : byte;
    session_id : bytes[length];
  };

  ++type CipherSuites = struct {
    length : word16;
    cipher_suites : word16[length / 2];
  };

  ++type CompressionMethods = struct {
    length : byte;
    compression_methods : byte[length];
  };

  ++type Extension = struct {
    type : word16;
    length : word16;
    data : bytes[length];
  };

  ++type Extensions = struct {
    length : word16;
    extensions : Extension[length to end];
  };

  public ++type TLSClientHello = struct {
    record_type : byte;
    version : ProtocolVersion;
    length : word16;
    handshake_type : byte;
    handshake_length : word24;
    handshake_version : ProtocolVersion;
    random : Random;
    session_id : SessionID;
    cipher_suites : CipherSuites;
    compression_methods : CompressionMethods;
    extensions : Extensions;
  };
}