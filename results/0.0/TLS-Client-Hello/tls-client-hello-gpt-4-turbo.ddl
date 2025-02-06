module TLS_ClientHello;

type uint8 = UInt : 8;
type uint16 = UInt : 16;

type opaque (len : UInt) = Bytes : len;

type Vec (elemType, lenType) = {
  len : lenType;
  elems : [elemType] : len;
};

struct ClientHello {
  legacy_version : uint16 = 0x0303; // TLS 1.2 for compatibility
  random : Bytes : 32;
  session_id : opaque(uint8);
  cipher_suites : Vec(uint16, uint16);
  legacy_compression_methods : Vec(uint8, uint8); // Should be 0x00
  extensions : Vec(Extension, uint16);
}

struct Extension {
  extension_type : uint16;
  extension_data : opaque(uint16);
}

// Example extension structures
struct ServerNameList {
  list : Vec(ServerName, uint16);
}

struct ServerName {
  name_type : uint8; // 0 for hostname
  name : opaque(uint16);
}

struct SupportedVersions {
  versions : Vec(uint16, uint8);
}

struct SignatureAlgorithms {
  supported_algorithms : Vec(SignatureScheme, uint16);
}

struct SignatureScheme {
  scheme : uint16;
}

struct SupportedGroups {
  named_groups : Vec(uint16, uint16);
}

struct KeyShareEntry {
  group : uint16;
  key_exchange : opaque(uint16);
}

struct KeyShare {
  client_shares : Vec(KeyShareEntry, uint16);
}

struct PskKeyExchangeModes {
  ke_modes : Vec(uint8, uint8);
}

struct EarlyDataIndication {}

struct Cookie {
  cookie : opaque(uint16);
}

// Extension parsing based on type
instance Extension.parse {
  switch (this.extension_type) {
    case 0x0000: this.extension_data as ServerNameList;
    case 0x002b: this.extension_data as SupportedVersions;
    case 0x000d: this.extension_data as SignatureAlgorithms;
    case 0x000a: this.extension_data as SupportedGroups;
    case 0x0033: this.extension_data as KeyShare;
    case 0x002d: this.extension_data as PskKeyExchangeModes;
    case 0x002f: this.extension_data as EarlyDataIndication;
    case 0x0010: this.extension_data as Cookie;
    default: this.extension_data as opaque(uint16);
  }
}