ClientHello : structure {
    ProtocolVersion : uint16;
    Random : opaque[32];
    SessionID : opaque[uint8];
    CipherSuites : list of CipherSuite (uint16);
    CompressionMethods : opaque[uint8];
    Extensions : list of Extension (uint16);
}

CipherSuite : structure {
    CipherSuiteBytes : opaque[2];
}

Extension : structure {
    ExtensionType : uint16;
    ExtensionData : opaque[uint16];
}

SupportedVersions : structure {
    SupportedVersionsList : opaque[uint8];
}

SupportedGroups : structure {
    NamedGroupList : list of uint16 (uint16);
}

SignatureAlgorithms : structure {
    SignatureSchemeList : list of uint16 (uint16);
}

KeyShare : structure {
    KeyShareList : list of KeyShareEntry (uint16);
}

KeyShareEntry : structure {
    Group : uint16;
    KeyExchange : opaque[uint16];
}

PSKKeyExchangeModes : structure {
    PSKKeyExchangeModeList : opaque[uint8];
}

PreSharedKey : structure {
    Identifiers : list of PskIdentity (uint16);
    Binders : list of PskBinderEntry (uint16);
}

PskIdentity : structure {
    Identity : opaque[uint16];
    ObfuscatedTicketAge : uint32;
}

PskBinderEntry : structure {
    Binder : opaque[uint8];
}

ServerName : structure {
    NameList : list of ServerNameList (uint16);
}

ServerNameList : structure {
    NameType : uint8;
    HostName : opaque[uint16];
}

ApplicationLayerProtocolNegotiation : structure {
    Protocols : list of ProtocolNameList (uint16);
}

ProtocolNameList : structure {
    ProtocolName : opaque[uint8];
}

MaxFragmentLength : structure {
    MaxFragmentLength : uint8;
}

StatusRequest : structure {
    StatusType : uint8;
    ResponderIDList : list of ResponderIDList (uint16);
    RequestExtensions : opaque[uint16];
}

ResponderIDList : structure {
    ResponderID : opaque[uint16];
}

SignatureAlgorithmsCert : structure {
    SignatureSchemeList : list of uint16 (uint16);
}

RecordSizeLimit : structure {
    RecordSizeLimit : uint16;
}

Cookie : structure {
    Cookie : opaque[uint16];
}

EarlyData : structure {
    MaxEarlyDataSize : uint32;
}

RenegotiationInfo : structure {
    RenegotiatedConnection : opaque[uint8];
}