TLSClientHello = {
  legacy_version: uint8[2],
  random: uint8[32],
  legacy_session_id: uint8[0..32],
  cipher_suites: CipherSuites,
  legacy_compression_methods: uint8[1] where $ == 0x00,
  extensions: Extensions
}

CipherSuites = {
  length: uint16,
  suites: CipherSuite[length / 2]
}

CipherSuite = {
  suite: uint16
}

Extensions = {
  length: uint16,
  extensions: Extension[length]
}

Extension = {
  type: uint16,
  length: uint16,
  data: uint8[length]
}

SupportedVersionsExtension = {
  versions: uint8[2]
}

KeyShareExtension = {
  shares: KeyShareEntry[]
}

KeyShareEntry = {
  group: uint16,
  key_exchange: uint8[]
}

SignatureAlgorithmsExtension = {
  algorithms: SignatureScheme[]
}

SignatureScheme = {
  scheme: uint16
}

PSKKeyExchangeModesExtension = {
  modes: uint8[]
}

PreSharedKeyExtension = {
  identities: PSKIdentity[],
  binders: PSKBinder[]
}

PSKIdentity = {
  identity: uint8[],
  obfuscated_ticket_age: uint32
}

PSKBinder = {
  binder: uint8[]
}

ServerNameExtension = {
  names: ServerName[]
}

ServerName = {
  type: uint8 where $ == 0x00,
  length: uint16,
  name: uint8[length]
}

SupportedGroupsExtension = {
  groups: uint16[]
}