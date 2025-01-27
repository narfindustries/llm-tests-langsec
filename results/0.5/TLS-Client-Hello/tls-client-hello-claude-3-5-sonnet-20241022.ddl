def Main = TLSClientHello

def TLSClientHello = {
    handshakeType : uint8
    length        : uint24
    version       : ProtocolVersion
    random        : Random
    sessionID     : SessionID
    cipherSuites  : CipherSuites
    comprMethods  : CompressionMethods
    extensions    : Extensions?
}

def ProtocolVersion = {
    major : uint8
    minor : uint8
}

def Random = {
    gmtUnixTime : uint32
    randomBytes : Array 28 uint8
}

def SessionID = {
    length : uint8
    id     : Array $length uint8
}

def CipherSuites = {
    length : uint16
    suites : Array ($length / 2) uint16
}

def CompressionMethods = {
    length  : uint8
    methods : Array $length uint8
}

def Extensions = {
    length     : uint16
    extensions : ExtensionList[$length]
}

def ExtensionList[remaining: uint16] = {
    | remaining == 0 => Done
    | otherwise => {
        ext : Extension
        rest : ExtensionList[remaining - (4 + ext.length)]
    }
}

def Extension = {
    type   : uint16
    length : uint16
    data   : Array $length uint8
}

def Done = null

def uint24 = {
    b1 : uint8
    b2 : uint8
    b3 : uint8
    Value ((b1 << 16) | (b2 << 8) | b3)
}