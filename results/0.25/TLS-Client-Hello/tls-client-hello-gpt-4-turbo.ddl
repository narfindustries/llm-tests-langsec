module TLS.ClientHello {
  import DAEDALUS::Core

  type Version = struct {
    major : UInt8
    minor : UInt8
  }

  type Random = struct {
    gmt_unix_time : UInt32
    random_bytes  : Bytes<28>
  }

  type SessionID = struct {
    length : UInt8
    session_id : Bytes<length>
  }

  type CipherSuite = struct {
    cipher_suites_length : UInt16
    cipher_suites : List<CipherSuiteItem, cipher_suites_length / 2>
  }

  type CipherSuiteItem = struct {
    item : UInt16
  }

  type CompressionMethod = struct {
    compression_methods_length : UInt8
    compression_methods : List<UInt8, compression_methods_length>
  }

  type Extension = struct {
    extension_type : UInt16
    extension_length : UInt16
    extension_data : Bytes<extension_length>
  }

  type Extensions = struct {
    extensions_length : UInt16
    extensions : List<Extension, compute_extensions_count(extensions_length)>
  }

  // Helper function to compute the number of extensions based on the total length of extensions data
  function compute_extensions_count(length: UInt16) -> UInt32 {
    var count : UInt32 = 0
    var total_length : UInt16 = 0
    while total_length < length {
      total_length += peek(UInt16) + 2 + 2 // extension_type + extension_length + length of extension_data
      count += 1
    }
    return count
  }

  type ClientHello = struct {
    version : Version
    random : Random
    session_id : SessionID
    cipher_suites : CipherSuite
    compression_methods : CompressionMethod
    extensions : Extensions
  }

  type TLSPlaintext = struct {
    content_type : UInt8
    version : Version
    length : UInt16
    fragment : ClientHello
  }
}