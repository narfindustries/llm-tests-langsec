def Main = TLS_client_hello

def TLS_client_hello = {
    §content_type = 0x16§
    §version = HandshakeVersion§
    §length = VarNum§
    §msg_type = 0x01§
    §size = VarNum§
    §client_version = HandshakeVersion§
    random: Random
    session_id: SessionID
    cipher_suites: CipherSuites
    compression_methods: CompressionMethods
    extensions: Extensions
}

def HandshakeVersion = {
    major : uint8
    minor : uint8
}

def VarNum = {
    value: uint16
}

def Random = {
    gmt_unix_time: uint32
    random_bytes: Array 28 uint8
}

def SessionID = {
    length: uint8
    session_id: Array $length uint8
}

def CipherSuites = {
    length: uint16
    cipher_suites: Array ($length / 2) uint16
}

def CompressionMethods = {
    length: uint8
    compression_methods: Array $length uint8
}

def Extensions = {
    length: uint16
    extensions: Array Many ExtensionEntry
}

def ExtensionEntry = {
    type: uint16
    length: uint16
    data: Array $length uint8
}

def Many (p: ParserMain) = {
    Count = length
    values = FSeq Count p
}

def FSeq n (p: ParserMain) = {
    values = for i <- [0 .. n-1]; yield p
}