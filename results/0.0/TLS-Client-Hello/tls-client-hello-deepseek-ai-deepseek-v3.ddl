ClientHello = {
  ProtocolVersion: uint16,
  Random: bytes[32],
  LegacySessionId: varuint[uint8],
  CipherSuites: varuint[uint16],
  LegacyCompressionMethods: varuint[uint8],
  Extensions: varuint[Extension]
};

Extension = {
  ExtensionType: uint16,
  ExtensionData: varuint[bytes]
};

SupportedVersions = {
  Versions: varuint[uint16]
};

KeyShare = {
  KeyShareEntries: varuint[KeyShareEntry]
};

KeyShareEntry = {
  Group: uint16,
  KeyExchange: varuint[bytes]
};

SignatureAlgorithms = {
  Algorithms: varuint[uint16]
};

SupportedGroups = {
  NamedGroupList: varuint[uint16]
};

ServerName = {
  ServerNameList: varuint[Name]
};

Name = {
  NameType: uint8,
  Name: varuint[bytes]
};

ALPN = {
  ProtocolNameList: varuint[ProtocolName]
};

ProtocolName = {
  Length: uint8,
  Name: bytes[Length]
};

PreSharedKey = {
  Identities: varuint[PSKIdentity],
  Binders: varuint[PSKBinderEntry]
};

PSKIdentity = {
  Identity: varuint[bytes],
  ObfuscatedTicketAge: uint32
};

PSKBinderEntry = {
  Binder: varuint[bytes]
};

EarlyData = {
  Empty: null
};

PostHandshakeAuth = {
  Empty: null
};

Cookie = {
  Cookie: varuint[bytes]
};

Padding = {
  Padding: varuint[bytes]
};