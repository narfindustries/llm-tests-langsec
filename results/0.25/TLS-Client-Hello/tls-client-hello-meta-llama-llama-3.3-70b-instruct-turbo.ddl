domain TLS_Client_Hello {
  importRFC5246

  syntax = TLS_Resource

  resources {
    TLS_Resource : record {
      uint8[1] type
      uint16[2] version
      uint8[1] compression_method
      uint8[1] cipher_suite
      uint8[2] random_session_id_length
      uint8[random_session_id_length] random_session_id
      Extensions extensions
    }

    Extensions : sequence {
      Extension extension
    }

    Extension : record {
      uint16[2] type
      uint16[2] length
      uint8[length] data
    }
  }

  actions {
    start(TLS_Resource resource) {
      verify(type == 1, "Invalid type")
      verify(version == 0x0303, "Invalid version")
      verify(compression_method == 0, "Invalid compression method")
      verify(cipher_suite == 0x0005, "Invalid cipher suite")
      verify(random_session_id_length == 32, "Invalid session ID length")
    }
  }
}