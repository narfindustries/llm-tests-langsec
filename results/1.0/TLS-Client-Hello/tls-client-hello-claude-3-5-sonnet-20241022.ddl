def TLS_Version = u16

def Random = { byte[32] }

def Length8 = u8

def Length16 = u16

def CipherSuite = u16

def ExtensionType = u16

def Extension = {
    extension_type: ExtensionType
    extension_length: Length16
    extension_data: byte[extension_length]
}

def ClientHello = {
    protocol_version: TLS_Version
    random: Random
    session_id_length: Length8
    session_id: byte[session_id_length]
    cipher_suites_length: Length16
    cipher_suites: CipherSuite[cipher_suites_length div 2]
    compression_methods_length: Length8
    compression_methods: byte[compression_methods_length]
    extensions_length: Length16
    extensions: Extension[]
}

def TLSRecord = {
    record_type: byte
    record_version: TLS_Version
    record_length: Length16
    client_hello: ClientHello
}

def Main = TLSRecord