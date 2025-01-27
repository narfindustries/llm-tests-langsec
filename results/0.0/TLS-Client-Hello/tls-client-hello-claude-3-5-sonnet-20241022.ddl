def Main = TLSClientHello

def TLSClientHello = {
    handshakeType : uint8
    length : uint24
    version : uint16
    random : Random
    sessionIDLength : uint8
    sessionID : Vector sessionIDLength uint8
    cipherSuitesLength : uint16
    cipherSuites : Vector (cipherSuitesLength / 2) uint16
    compressionMethodsLength : uint8
    compressionMethods : Vector compressionMethodsLength uint8
    extensionsLength : uint16
    extensions : Vector extensionsLength uint8
    $$ handshakeType == 1
}

def Random = {
    gmtUnixTime : uint32
    randomBytes : Vector 28 uint8
}

def uint8  = IntBE 1
def uint16 = IntBE 2
def uint24 = IntBE 3
def uint32 = IntBE 4