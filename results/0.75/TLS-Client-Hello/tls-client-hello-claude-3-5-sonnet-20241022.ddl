def TLSClientHello = {
    uint8 handshakeType
    uint24 length
    uint16 legacyVersion
    byte[32] random
    uint8 sessionIdLength
    byte[sessionIdLength] legacySessionId
    uint16 cipherSuitesLength
    struct[cipherSuitesLength/2] cipherSuites = {
        uint16 cipherSuite
    }
    uint8 compressionMethodsLength
    byte[compressionMethodsLength] compressionMethods
    uint16 extensionsLength
    struct[extensionsLength] extensions = {
        uint16 extensionType
        uint16 extensionLength
        byte[extensionLength] extensionData
    }
}

def SNIExtension = {
    uint16 serverNameListLength
    struct[serverNameListLength] serverNameList = {
        uint8 nameType
        uint16 nameLength
        byte[nameLength] hostName
    }
}

def SupportedGroupsExtension = {
    uint16 supportedGroupsLength
    struct[supportedGroupsLength/2] groups = {
        uint16 namedGroup
    }
}

def SignatureAlgorithmsExtension = {
    uint16 signatureAlgorithmsLength
    struct[signatureAlgorithmsLength/2] algorithms = {
        uint16 signatureScheme
    }
}

def KeyShareExtension = {
    uint16 clientSharesLength
    struct[clientSharesLength] clientShares = {
        uint16 group
        uint16 keyExchangeLength
        byte[keyExchangeLength] keyExchange
    }
}

def SupportedVersionsExtension = {
    uint8 versionsLength
    struct[versionsLength/2] versions = {
        uint16 version
    }
}

def PreSharedKeyExtension = {
    uint16 identitiesLength
    struct[identitiesLength] identities = {
        uint16 identityLength
        byte[identityLength] identity
        uint32 obfuscatedTicketAge
    }
    uint16 bindersLength
    struct[bindersLength] binders = {
        uint8 binderLength
        byte[binderLength] binderEntry
    }
}

def PSKKeyExchangeModesExtension = {
    uint8 keModeLength
    byte[keModeLength] keModes
}

def EarlyDataExtension = {
}

def CookieExtension = {
    uint16 cookieLength
    byte[cookieLength] cookie
}

def CertificateAuthoritiesExtension = {
    uint16 authoritiesLength
    struct[authoritiesLength] authorities = {
        uint16 dnLength
        byte[dnLength] distinguishedName
    }
}

def PostHandshakeAuthExtension = {
}

def SignatureAlgorithmsCertExtension = {
    uint16 signatureAlgorithmsLength
    struct[signatureAlgorithmsLength/2] algorithms = {
        uint16 signatureScheme
    }
}

def PaddingExtension = {
    byte[extensionLength] paddingData
}