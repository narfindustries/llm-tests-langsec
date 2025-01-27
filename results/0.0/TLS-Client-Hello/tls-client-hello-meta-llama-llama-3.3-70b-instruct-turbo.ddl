domain TLS_Client_Hello {
  import "TLS-Constants.ddl";

  type TLS_Client_Hello {
    uint8(1) record_content_type;
    uint16(2) version;
    uint16(2) length;
    TLS_Handshake handshake;
  }

  type TLS_Handshake {
    uint8(1) handshake_type;
    uint24(3) length;
    switch (handshake_type) {
      case client_hello: TLS_Client_Hello_Message;
    }
  }

  type TLS_Client_Hello_Message {
    uint16(2) version;
    uint8(1) random_session_id_length;
    bytes(random_session_id_length) random_session_id;
    uint16(2) cipher_suites_length;
    uint16(cipher_suites_length / 2) cipher_suites;
    uint8(1) compression_methods_length;
    uint8(compression_methods_length) compression_methods;
    uint8(1) extensions_length;
    TLS_Extensions extensions;
  }

  type TLS_Extensions {
    repeat {
      uint16(2) extension_type;
      uint16(2) extension_length;
      switch (extension_type) {
        case server_name: TLS_Extension_Server_Name;
        case supported_versions: TLS_Extension_Supported_Versions;
      }
    }
  }

  type TLS_Extension_Server_Name {
    uint16(2) server_name_list_length;
    repeat {
      uint16(2) server_name_type;
      uint16(2) server_name_length;
      bytes(server_name_length) server_name;
    }
  }

  type TLS_Extension_Supported_Versions {
    uint8(1) supported_versions_length;
    repeat {
      uint16(2) version;
    }
  }
}